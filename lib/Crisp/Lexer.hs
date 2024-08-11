module Crisp.Lexer (
  lexer,
  CrispLexeme (..),
  type CrispLexemes,
  type CrispLexemesWithOffset,
  type CrispLexemeWithOffset,
  Delimiter,
) where

import Control.Applicative (asum)
import Control.Arrow ((&&&))
import Control.Exception (Exception)
import Control.Lens (
  both,
  to,
  traversed,
  view,
  (^..),
 )
import Control.Monad (void)
import Crisp.Utils (
  lineNumberLens,
  parse,
  throwErrorToIOFinal,
  type TParseError,
  type TParser,
 )
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Polysemy (
  runFinal,
 )
import Text.Megaparsec (
  MonadParsec (eof, label),
 )
import Text.Megaparsec qualified as MP
import Text.Megaparsec qualified as MPCL
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPCL
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)

type Parser a = TParser Text a
type ParserError = TParseError Text

---

data Delimiter where
  DParenthesis :: Delimiter
  DBrace :: Delimiter
  DBracket :: Delimiter
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)

delimiterChars :: Delimiter -> (Char, Char)
delimiterChars DParenthesis = ('(', ')')
delimiterChars DBrace = ('[', ']')
delimiterChars DBracket = ('{', '}')

data CrispLexeme where
  CLLDelimiter :: Delimiter -> CrispLexeme
  CLRDelimiter :: Delimiter -> CrispLexeme
  CLSpace :: CrispLexeme
  CLSymbol :: String -> CrispLexeme
  CLNumber :: Int -> CrispLexeme
  CLQuote :: CrispLexeme
  CLUnquote :: CrispLexeme
  CLDiscard :: Int -> CrispLexeme
  deriving stock (Eq, Ord, Show, Generic)

type CrispLexemeWithOffset = (CrispLexeme, (Int, Int))

type CrispLexemes a = [(CrispLexeme, a)]

type CrispLexemesWithOffset = CrispLexemes (Int, Int)

---

withOffset :: Parser a -> Parser (a, (Int, Int))
withOffset p = do
  -- Capture the offset before running the parser
  offsetBefore <- view lineNumberLens <$> MP.getParserState
  -- Run the actual parser
  result <- p
  -- Capture the offset after running the parser
  offsetAfter <- view lineNumberLens <$> MP.getParserState
  -- Construct the Cofree structure with the offsets
  -- pure $ mapped .~ (offsetBefore, offsetAfter) $ result
  pure $ (result, (offsetBefore, offsetAfter))

symbolP :: Parser CrispLexeme
symbolP = label "symbol" do
  symbol <- MP.some MPC.letterChar
  pure $ CLSymbol symbol

numberP :: Parser CrispLexeme
numberP = label "number" do
  digits <- MP.some digitP
  let result = foldl ((+) . (10 *)) 0 digits
  pure $ CLNumber result

digitP :: Parser Int
digitP = MP.token (readMaybe . Text.unpack . Text.singleton) mempty

-- whitespaceP :: Parser ()
-- whitespaceP =
--   MPCL.space
--     (MPC.space1)
--     (MPCL.skipLineComment ";")
--     (MPCL.skipBlockCommentNested "(*" "*)")

whitespaceP :: Parser CrispLexeme
whitespaceP =
  ( CLSpace
      <$
  )
    $ MPCL.skipSome
    $ MP.choice
    $ MP.hidden
      <$> [MPC.space1, MPCL.skipLineComment ";", MPCL.skipBlockCommentNested "(*" "*)"]

allDelimiters :: [Delimiter]
allDelimiters = [minBound .. maxBound]

delimiterP :: Parser CrispLexeme
delimiterP =
  MP.choice $
    allDelimiters
      ^.. traversed
        . to (id &&& delimiterChars)
        . to
          ( \(d, (l, r)) ->
              ( CLLDelimiter d <$ MPC.char l
              , CLRDelimiter d <$ MPC.char r
              )
          )
        . both

-- >>> [(1, 2), (3, 4)] ^.. traverse . both . to succ
-- [2,3,4,5]

quoteP :: Parser CrispLexeme
quoteP = label "quote" do
  void $ MPC.char '`'
  -- MP.notFollowedBy whitespaceP
  pure $ CLQuote

unquoteP :: Parser CrispLexeme
unquoteP = label "unquote" do
  void $ MPC.char ','
  -- MP.notFollowedBy whitespaceP
  pure $ CLUnquote

discardP :: Parser CrispLexeme
discardP = label "discard" do
  times <- length <$> MP.try (MPC.string "#_" `MP.sepBy1` MP.optional whitespaceP)
  pure $ CLDiscard $ times

lexeme :: Parser CrispLexeme
lexeme =
  asum
    [ whitespaceP
    , delimiterP
    , quoteP
    , unquoteP
    , symbolP
    , numberP
    , discardP
    ]

lexer :: Parser CrispLexemesWithOffset
lexer = MP.many (withOffset lexeme) <* eof

kekL :: IO ()
kekL = do
  code <- Text.pack <$> getLine
  lexemes <-
    runFinal
      . throwErrorToIOFinal
      -- \$ fmap (metadata .~ ())
      $ parse lexer "file" code
  pPrint lexemes
