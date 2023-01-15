{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}

module ParserCombinators.Parser (
  token,
  satisfy,
  eof,
  char,
  string,
  between,
  space,
  spaces,
  spaces1,
  bool,
  digit,
  integer,
  symbol,
  many1,
  sepBy,
  sepBy1,
) where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Foldable (asum)

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import ParserCombinators.Datatypes (
  ParseError (ParseError),
  ParseErrorType (
    CustomError,
    EndOfInput,
    Expected,
    ExpectedEndOfFile,
    Unexpected
  ),
  Parser (Parser),
 )
import Text.Read (readMaybe)

-- | Parse a token/character using a custom error generator and condition
token :: (i -> ParseErrorType i e) -> (i -> Maybe a) -> Parser [i] [] e a
token mkErr predicate = Parser $ \input offset ->
  case input of
    [] -> Left $ pure $ ParseError offset EndOfInput
    x : rest
      | Just a <- predicate x -> Right (succ offset, a, rest)
      | otherwise -> Left $ pure $ ParseError offset $ mkErr x

-- | Parse taken/character by a condition
satisfy :: (i -> Maybe a) -> Parser [i] [] e a
satisfy = token Unexpected

-- | Parse EOF
eof :: Parser [i] [] e ()
eof = Parser $ \input offset ->
  case input of
    [] -> Right (offset, (), [])
    x : _ -> Left [ParseError offset $ ExpectedEndOfFile x]

-- | Parse a character
char :: Eq i => i -> Parser [i] [] e i
char i = token (Expected i) (ensure (== i))
  where
    ensure :: (a -> Bool) -> a -> Maybe a
    ensure p x
      | p x = Just x
      | otherwise = Nothing

-- | Parse a string/symbol
string :: Eq i => [i] -> Parser [i] [] e [i]
string = traverse char

-- | Parse between the specified open and close parsers
between :: Parser [i] m e a -> Parser [i] m e b -> Parser [i] m e c -> Parser [i] m e c
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

-- | Parse one space, ignoring the result
space :: Parser String [] e ()
space = void $ asum $ char <$> " \n\t"

-- | Parse zero or more spaces, ignoring the result
spaces :: Parser String [] e ()
spaces = void $ many $ space

-- | Parse one or more spaces, ignoring the result
spaces1 :: Parser String [] e ()
spaces1 = void $ some $ space

-- | Parse a boolean true or false
bool :: Parser String [] e Bool
bool =
  asum
    [ True <$ string "true"
    , False <$ string "false"
    ]

-- | Parse a digit
digit :: Parser String [] e Integer
digit = satisfy (readMaybe . pure)

-- | Parse an integer
integer :: Parser String [] e Integer
integer = foldl' (\res x -> res * 10 + x) 0 <$> some digit

-- | Parse a symbol
symbol :: Parser String [] String String
symbol = some $ token forbidden validate
  where
    forbidden i = CustomError $ "Forbidden symbol character " ++ show i
    validate c =
      if isAlphaNum c
        then Just c
        else Nothing

-- | Parse one or more of the specified parser
many1 :: (Alternative m) => Parser [i] m e a -> Parser [i] m e (NonEmpty a)
many1 p = do
  x <- p
  xs <- many p
  return $ x :| xs

-- | Parse a list of items separated by the specified separator parser
sepBy :: (Alternative m) => Parser [i] m e a -> Parser [i] m e b -> Parser [i] m e [a]
sepBy p sep = toList <$> sepBy1 p sep <|> pure []

-- | Parse a non-empty list of items separated by the specified separator parser
sepBy1 :: (Alternative m) => Parser [i] m e a -> Parser [i] m e b -> Parser [i] m e (NonEmpty a)
sepBy1 p sep = do
  x <- p
  xs <- many (sep *> p)
  return $ x :| xs
