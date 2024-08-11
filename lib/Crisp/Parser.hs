{-# OPTIONS_GHC -Wno-orphans #-}

module Crisp.Parser (
  kekP,
) where

import Control.Applicative (asum)
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Lens (
  Field1 (_1),
  Prism',
  matching',
  preview,
  prism',
  review,
 )
import Control.Monad (guard, replicateM_, void, (>=>))
import Crisp.Lexer (
  CrispLexeme (..),
  Delimiter,
  lexer,
  type CrispLexemesWithOffset,
 )
import Crisp.Utils (
  expected,
  parse,
  sepBy1,
  throwErrorToIOFinal,
  type TParseError,
  type TParser,
 )
import Data.Deriving (deriveShow1)
import Data.Either.Extra (eitherToMaybe)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Polysemy (
  runFinal,
 )
import Text.Megaparsec (
  MonadParsec (label),
 )
import Text.Megaparsec qualified as MP
import Text.Pretty.Simple (pPrint)

type Parser a = TParser CrispLexemesWithOffset a
type ParseError = TParseError Text

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
--   :: (Member (Error ParseError) r)
--   => (Member (State (MP.State Text Void)) r)
--   => Parser a
--   -> Text
--   -> Sem r a
-- parse' parser code = (\(a, b) -> put a >> firstA throw b) $ MP.runParser' parser (MP.initial code)

data CrispExprF r where
  CrispSymbol :: String -> CrispExprF r
  CrispNumber :: Int -> CrispExprF r
  CrispSexpr
    :: { delimiter :: Delimiter
       , expressions :: NonEmpty r
       }
    -> CrispExprF r
  CrispQuote :: r -> CrispExprF r
  CrispUnquote :: r -> CrispExprF r

deriving stock instance Functor CrispExprF
deriving stock instance Foldable CrispExprF

$(deriveShow1 ''CrispExprF)

type CrispExprWithOffset = Cofree CrispExprF (Int, Int)

parseAnnotatedLexeme
  :: forall
    -- V Lexeme type
    (lexeme :: Type)
    -- V Metadata type
    (meta :: Type)
    -- V Lexeme data type
    (lexdata :: Type)
    -- V ExpressionF data type
    (final :: Type)
    -- V Parser error type
    (err :: Type)
   . (Ord lexeme, Ord meta, Ord err)
  => Prism' lexeme lexdata
  -- ^ Prism for matching the exact lexeme
  -> (lexdata -> meta -> final)
  -- ^ Constructor for creating the final expression from the lexeme data and metadata
  -> MP.Parsec err [(lexeme, meta)] final
  -- ^ Parser that parses one annotated lexeme and creates the matching expression
parseAnnotatedLexeme p c =
  uncurry c
    <$> MP.token
      (eitherToMaybe . matching' (fstPrism p))
      mempty
  where
    fstPrism :: Prism' a a' -> Prism' (a, b) (a', b)
    fstPrism pa =
      prism'
        (\(a', b) -> (,b) $ review pa a')
        (\(a, b) -> (,b) <$> preview pa a)

parseAnnotatedLexeme'
  :: forall
    -- V Lexeme type
    (lexeme :: Type)
    -- V Metadata type
    (meta :: Type)
    -- V Lexeme data type
    (lexdata :: Type)
    -- V Parser error type
    (err :: Type)
    -- V ExpressionF data type
    (exprf :: Type -> Type)
   . (Ord lexeme, Ord meta, Ord err)
  => Prism' lexeme lexdata
  -- ^ Prism for matching the exact lexeme
  -> (forall r. lexdata -> exprf r)
  -- ^ Constructor for creating the (nonrecursive) expression from the lexeme data
  -> MP.Parsec err [(lexeme, meta)] (Cofree exprf meta)
  -- ^ Parser that parses one annotated lexeme and creates the matching expression
parseAnnotatedLexeme' p c =
  carryMetadata c
    <$> MP.token
      (eitherToMaybe . matching' (fstPrism p))
      mempty
  where
    fstPrism :: Prism' a a' -> Prism' (a, b) (a', b)
    fstPrism pa =
      prism'
        (\(a', b) -> (,b) $ review pa a')
        (\(a, b) -> (,b) <$> preview pa a)

    carryMetadata :: (forall r. a -> b r) -> (a, m) -> Cofree b m
    carryMetadata f (a, m) = m :< f a

symbolP :: Parser CrispExprWithOffset
symbolP = parseAnnotatedLexeme' #_CLSymbol CrispSymbol

numberP :: Parser CrispExprWithOffset
numberP = parseAnnotatedLexeme' #_CLNumber CrispNumber

whitespaceP :: Parser ()
whitespaceP = parseAnnotatedLexeme #_CLSpace (const (const ()))

discardP :: Parser ()
discardP =
  parseAnnotatedLexeme #_CLDiscard const
    >>= (\times -> replicateM_ times $ MP.try (expressionP <* MP.try (MP.optional whitespaceP)))

sexprP :: Parser CrispExprWithOffset
sexprP = label "sexpr" do
  (delimiter, start) <- parseAnnotatedLexeme #_CLLDelimiter (,)
  MP.notFollowedBy whitespaceP
  expressions <- do
    expressionP `sepBy1` MP.some (whitespaceP MP.<|> discardP)
  void $ MP.optional (whitespaceP >> discardP)
  MP.notFollowedBy whitespaceP
  MP.token (preview (_1 . #_CLRDelimiter) >=> (guard . (== delimiter))) $ expected (CLRDelimiter delimiter, start)
  -- pure $ (start ^. _1, end ^. _2) :< CrispSexpr {delimiter, expressions}
  pure $ start :< CrispSexpr {delimiter, expressions}

expressionP :: Parser CrispExprWithOffset
expressionP =
  asum
    [ symbolP
    , numberP
    , sexprP
    ]

parser :: Parser (NonEmpty CrispExprWithOffset)
parser = (expressionP `sepBy1` whitespaceP) <* MP.eof

kekP :: IO ()
kekP = do
  let file = "file"
  code <- Text.pack <$> getLine
  lexemes <-
    runFinal
      . throwErrorToIOFinal
      $ parse lexer file code
  ast <-
    runFinal
      . throwErrorToIOFinal
      $ parse parser file lexemes
  pPrint ast

-- quotedP :: Parser CrispExprWithOffset
-- quotedP = label "quoted" do
--   offsetBefore <- view offsetLens <$> MP.getParserState
--   void $ MPC.char '`'
--   expression <- expressionP
--   offsetAfter <- view offsetLens <$> MP.getParserState
--   pure $ (offsetBefore, offsetAfter) :< CrispQuote expression
--
-- unquotedP :: Parser CrispExprWithOffset
-- unquotedP = label "unquoted" do
--   offsetBefore <- view offsetLens <$> MP.getParserState
--   void $ MPC.char ','
--   expression <- expressionP
--   offsetAfter <- view offsetLens <$> MP.getParserState
--   pure $ (offsetBefore, offsetAfter) :< CrispUnquote expression
--
-- data CrispFile e where
--   CrispFile
--     :: { expressions :: [Cofree CrispExprF e]
--        }
--     -> CrispFile e
--   deriving stock (Generic, Show)
--
-- metadata :: Setter (CrispFile a) (CrispFile b) a b
-- metadata = #expressions . traversed . mapped
--
-- fileP :: Parser (CrispFile (Int, Int))
-- fileP = label "file" do
--   expressions <- expressionP `MP.sepBy` whitespace
--   pure $ CrispFile {expressions}
--
-- showCrisp :: CrispExprF () -> String
-- showCrisp e = case e of {}
