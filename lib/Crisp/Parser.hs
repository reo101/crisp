{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Crisp.Parser (
  kek,
) where

import Control.Applicative (Alternative (empty), asum)
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Exception (Exception, throwIO)
import Control.Lens (Lens', iso, view, (.~))
import Control.Monad (join, void)
import Data.Bifunctor (first)
import Data.Char (toUpper)
import Data.Complex (Complex (..))
import Data.Composition ((.:))
import Data.Deriving (deriveShow1)
import Data.Function ((&))
import Data.Functor.Classes (Show1)
import Data.GADT.Compare (GEq (geq))
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List qualified as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Type.Equality (type (:~:) (Refl))
import Data.Void (Void)
import Debug.Trace (traceId, traceShow)
import GHC.Generics (Generic)
import Polysemy (
  Embed,
  Final,
  Member,
  Members,
  Sem,
  embed,
  embedFinal,
  embedToFinal,
  interpret,
  interpretH,
  makeSem,
  reinterpret3,
  reinterpret3H,
  reinterpretH,
  runFinal,
  runT,
  runTSimple,
 )
import Polysemy.Error (Error, errorToIOFinal, fromEither, throw)
import Polysemy.Reader (Reader, ask)
import Polysemy.State (State, put)
import Text.Megaparsec (
  MonadParsec (eof, label, notFollowedBy, try),
  ParseErrorBundle,
  PosState (PosState),
  SourcePos (SourcePos, sourceColumn),
  Stream (Token),
  TraversableStream,
  (<|>),
 )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as MPB
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPCL
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)

-- data HList (ts :: [Type]) where
--   Nil :: HList '[]
--   (:>) :: t -> HList ts -> HList (t ': ts)
-- infixr 5 :>
--
-- data Elem ts t where
--   EZ :: Elem (t ': ts) t
--   ES :: Elem ts t -> Elem (t' ': ts) t
--
-- get :: Elem ts t -> HList ts -> t
-- get EZ (r :> _) = r
-- get (ES e) (_ :> rs) = get e rs
--
-- hlist :: HList '[Bool, Int, Maybe [String]]
-- hlist = True :> (5 :: Int) :> Just [("kek" :: String)] :> Nil

-- >>> get (ES $ EZ) hlist
-- 5

-- Define the type alias for your parser
type Parser a = MP.Parsec Void Text a

-- | A megaparsec error type
type ParserError = ParseErrorBundle Text Void

firstA :: (Applicative m) => (a -> m b) -> Either a b -> m b
firstA f = either f pure

parse
  :: (Member (Error ParserError) r)
  => Parser a
  -> Text
  -> Sem r a
parse parser code = firstA throw $ MP.parse parser "file" code

-- -- Function to create the initial parser state
-- createInitialState :: Text -> MP.State Text Void
-- createInitialState input =
--   MP.State
--     { stateInput = input
--     , stateOffset = 0
--     , statePosState =
--         PosState
--           { pstateInput = input
--           , pstateOffset = 0
--           , pstateSourcePos = MP.initialPos ""
--           , pstateTabWidth = MP.defaultTabWidth
--           , pstateLinePrefix = ""
--           }
--     , stateParseErrors = []
--     }

-- parse'
--   :: (Member (Error ParserError) r)
--   => (Member (State (MP.State Text Void)) r)
--   => Parser a
--   -> Text
--   -> Sem r a
-- parse' parser code = (\(a, b) -> put a >> firstA throw b) $ MP.runParser' parser (MP.initial code)

throwErrorToIOFinal
  :: (Exception e)
  => (Member (Final IO) r)
  => Sem (Error e : r) b
  -> Sem r b
throwErrorToIOFinal = (firstA (embedFinal . throwIO) =<<) . errorToIOFinal

data CrispExprF r where
  CrispSymbol :: String -> CrispExprF r
  CrispNumber :: Integer -> CrispExprF r
  CrispSexpr :: NonEmpty r -> CrispExprF r
  deriving stock (Functor, Foldable)

$(deriveShow1 ''CrispExprF)

type CrispExprWithOffset = Cofree CrispExprF (Int, Int)

-- | Parse a non-empty list of items separated by the specified separator parser
sepBy1 :: Parser a -> Parser b -> Parser (NonEmpty a)
sepBy1 p sep = do
  x <- p
  xs <- MP.many (sep *> p)
  return $ x :| xs

lineNumberLens :: Lens' (MP.State s e) Int
lineNumberLens = #statePosState . #pstateSourcePos . #sourceLine . iso MP.unPos MP.mkPos

offsetLens :: Lens' (MP.State s e) Int
offsetLens = #stateOffset

---

symbolP :: Parser CrispExprWithOffset
symbolP = label "symbol" do
  offsetBefore <- view offsetLens <$> MP.getParserState
  symbol <- MP.some MPC.letterChar
  offsetAfter <- view offsetLens <$> MP.getParserState
  pure $ (offsetBefore, offsetAfter) :< CrispSymbol symbol

numberP :: Parser CrispExprWithOffset
numberP = label "number" do
  offsetBefore <- view offsetLens <$> MP.getParserState
  digits <- MP.some digitP
  offsetAfter <- view offsetLens <$> MP.getParserState
  let result = foldl ((+) . (10 *)) 0 digits
  pure $ (offsetBefore, offsetAfter) :< CrispNumber result

digitP :: Parser Integer
digitP = MP.token (readMaybe . Text.unpack . Text.singleton) mempty

whitespace :: Parser ()
whitespace = MPCL.space (MPC.space1) (MPCL.skipLineComment ";") (MPCL.skipBlockCommentNested "(*" "*)")

sexprP :: Parser CrispExprWithOffset
sexprP = label "sexpr" do
  offsetBefore <- view offsetLens <$> MP.getParserState
  void $ MPC.char '('
  children <- expressionP `sepBy1` whitespace
  void $ MPC.char ')'
  offsetAfter <- view offsetLens <$> MP.getParserState
  pure $ (offsetBefore, offsetAfter) :< CrispSexpr children

expressionP :: Parser CrispExprWithOffset
expressionP =
  asum
    [ symbolP
    , numberP
    , sexprP
    ]

data CrispFile e where
  CrispFile
    :: { expressions :: [Cofree CrispExprF e]
       }
    -> CrispFile e
  deriving stock (Show)

fileP :: Parser (CrispFile (Int, Int))
fileP = label "file" do
  CrispFile <$> expressionP `MP.sepBy` whitespace

kek :: IO (CrispFile (Int, Int))
kek =
  runFinal
    . throwErrorToIOFinal @ParserError
    $ parse fileP "(type f (function int int int int)) (define (f a b c) (plus 1 b 4))"

-- >>> kek
-- CrispFile {expressions = [(0,31) :< CrispSexpr (((1,7) :< CrispSymbol "define") :| [(8,17) :< CrispSexpr (((9,10) :< CrispSymbol "f") :| [(11,12) :< CrispSymbol "a",(13,14) :< CrispSymbol "b",(15,16) :< CrispSymbol "c"]),(18,30) :< CrispSexpr (((19,23) :< CrispSymbol "plus") :| [(24,25) :< CrispNumber 1,(26,27) :< CrispSymbol "b",(28,29) :< CrispNumber 4])])]}
