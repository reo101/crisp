{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser
  ( ParseError (..),
    Parser (..),
    satisfy,
    char,
    string,
    token,
    eof,
  )
where

import Control.Applicative (Alternative (..))

data ParseErrorType i e
  = EndOfInput
  | Unexpected i
  | Expected i i
  | ExpectedEndOfFile i
  | CustomError e
  | Empty
  deriving (Eq)

instance (Show i, Show e) => Show (ParseErrorType i e) where
  show :: ParseErrorType i e -> String
  show EndOfInput = "End of Input"
  show (Unexpected a) = "Unexpected " ++ show a
  show (Expected e a) = "Expected " ++ show e ++ ", got " ++ show a
  show (ExpectedEndOfFile a) = "Expected EndOfFile, got " ++ show a
  show (CustomError e) = "Custom Error: " ++ show e
  show Empty = "No error"

type Offset = Integer

instance (Show i, Show e) => Show (ParseError i e) where
  show :: ParseError i e -> String
  show (ParseError o e) = "Error: " ++ show e ++ ", at offset " ++ show o

data ParseError i e where
  ParseError ::
    {errOffset :: Offset, errError :: ParseErrorType i e} ->
    ParseError i e

newtype Parser i m e a where
  Parser ::
    {runParser :: [i] -> Offset -> Either (m (ParseError i e)) (Offset, a, [i])} ->
    Parser i m e a

instance (Monoid (m (ParseError i e))) => Functor (Parser i m e) where
  fmap :: (a -> b) -> Parser i m e a -> Parser i m e b
  fmap f (Parser p) = Parser $ \input offset -> do
    (offset', output, rest) <- p input offset
    pure (succ offset', f output, rest)

instance (Monoid (m (ParseError i e))) => Applicative (Parser i m e) where
  pure :: a -> Parser i m e a
  pure a = Parser $ \input offset -> Right (offset, a, input)

  (<*>) :: Parser i m e (a -> b) -> Parser i m e a -> Parser i m e b
  Parser f <*> Parser p = Parser $ \input offset -> do
    (offset', f', rest) <- f input offset
    (offset'', output, rest') <- p rest offset'
    pure (offset'', f' output, rest')

instance (Monoid (m (ParseError i e))) => Monad (Parser i m e) where
  (>>=) :: Parser i m e a -> (a -> Parser i m e b) -> Parser i m e b
  (Parser p) >>= k = Parser $ \input offset -> do
    (offset', output, rest) <- p input offset
    runParser (k output) rest offset'

instance (Monoid (m (ParseError i e)), Alternative m) => Alternative (Parser i m e) where
  empty :: Parser i m e a
  empty = Parser $ \_ offset -> Left $ pure $ ParseError offset Empty

  (<|>) :: Parser i m e a -> Parser i m e a -> Parser i m e a
  (Parser l) <|> (Parser r) = Parser $ \input offset ->
    case l input offset of
      Left err_l ->
        case r input offset of
          Left err_r -> Left $ err_l <|> err_r
          Right (offset_r, output_r, rest_r) -> Right (offset_r, output_r, rest_r)
      Right (offset_l, output_l, rest_l) -> Right (offset_l, output_l, rest_l)

---

token :: (i -> ParseErrorType i e) -> (i -> Bool) -> Parser i [] e i
token mkErr predicate = Parser $ \input offset ->
  case input of
    [] -> Left $ pure $ ParseError offset EndOfInput
    x : rest
      | predicate x -> Right (succ offset, x, rest)
      | otherwise -> Left $ pure $ ParseError offset $ mkErr x

satisfy :: (i -> Bool) -> Parser i [] e i
satisfy = token Unexpected

eof :: Parser i [] e ()
eof = Parser $ \input offset ->
  case input of
    [] -> Right (offset, (), [])
    x : _ -> Left [ParseError offset $ ExpectedEndOfFile x]

char :: Eq i => i -> Parser i [] e i
char i = token (Expected i) (== i)

string :: Eq i => [i] -> Parser i [] e [i]
string = traverse char
