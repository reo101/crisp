{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}

module ParserCombinators.Parser
  ( token
  , satisfy
  , eof
  , char
  , string
  , between
  , space
  , spaces
  , spaces1
  , bool
  , digit
  , integer
  , symbol
  , many1
  , try
  , sepBy
  , sepBy1
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit)
import Data.Foldable (asum)

import ParserCombinators.Datatypes

-- | Parse a token/character using a custom error generator and condition
token :: (i -> ParseErrorType i e) -> (i -> Bool) -> Parser i [] e i
token mkErr predicate = Parser $ \input offset ->
  case input of
    [] -> Left $ pure $ ParseError offset EndOfInput
    x : rest
      | predicate x -> Right (succ offset, x, rest)
      | otherwise -> Left $ pure $ ParseError offset $ mkErr x

-- | Parse taken/character by a condition
satisfy :: (i -> Bool) -> Parser i [] e i
satisfy = token Unexpected

-- | Parse EOF
eof :: Parser i [] e ()
eof = Parser $ \input offset ->
  case input of
    [] -> Right (offset, (), [])
    x : _ -> Left [ParseError offset $ ExpectedEndOfFile x]

-- | Parse a character
char :: Eq i => i -> Parser i [] e i
char i = token (Expected i) (== i)

-- | Parse a string/symbol
string :: Eq i => [i] -> Parser i [] e [i]
string = mapM char

-- | Parse between the specified open and close parsers
between :: Parser i m e a -> Parser i m e b -> Parser i m e c -> Parser i m e c
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

-- | Parse one space, ignoring the result
space :: Parser Char [] e ()
space = void $ asum $ char <$> " \n\t"

-- | Parse zero or more spaces, ignoring the result
spaces :: Parser Char [] e ()
spaces = void $ many $ space

-- | Parse one or more spaces, ignoring the result
spaces1 :: Parser Char [] e ()
spaces1 = void $ some $ space

-- | Parse a boolean true or false
bool :: Parser Char [] e Bool
bool =
  asum
    [ True <$ string "true"
    , False <$ string "false"
    ]

-- | Parse a digit
digit :: Parser Char [] e Integer
digit = read . pure <$> satisfy isDigit

-- | Parse an integer
integer :: Parser Char [] e Integer
integer = foldl (\res x -> res * 10 + x) 0 <$> some digit

-- | Parse a symbol
symbol :: Parser Char [] String String
symbol = some $ token forbidden isValid
  where
    forbidden i = CustomError $ "Forbidden symbol character " ++ show i
    isValid c = isAlphaNum c

-- | Parse one or more of the specified parser
many1 :: (Alternative m) => Parser i m e a -> Parser i m e [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x : xs

-- | Try parsing the specified parser, returning an empty list if it fails
try :: (Alternative m) => Parser i m e a -> Parser i m e (Maybe a)
try p = Just <$> p <|> pure Nothing

-- | Parse a list of items separated by the specified separator parser
sepBy :: (Alternative m) => Parser i m e a -> Parser i m e b -> Parser i m e [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parse a non-empty list of items separated by the specified separator parser
sepBy1 :: (Alternative m) => Parser i m e a -> Parser i m e b -> Parser i m e [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep *> p)
  return $ x : xs
