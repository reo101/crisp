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
  )
where

import Control.Applicative (Alternative (..))

data ParseError i e
  = EndOfInput
  | Unexpected i
  | CustomError e
  | Empty
  deriving (Eq, Show)

newtype Parser i m e a where
  Parser ::
    {runParser :: [i] -> Either (m (ParseError i e)) (a, [i])} ->
    Parser i m e a

instance (Monoid (m (ParseError i e))) => Functor (Parser i m e) where
  fmap :: (a -> b) -> Parser i m e a -> Parser i m e b
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance (Monoid (m (ParseError i e))) => Applicative (Parser i m e) where
  pure :: a -> Parser i m e a
  pure a = Parser $ \input -> Right (a, input)

  (<*>) :: Parser i m e (a -> b) -> Parser i m e a -> Parser i m e b
  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance (Monoid (m (ParseError i e))) => Monad (Parser i m e) where
  (>>=) :: Parser i m e a -> (a -> Parser i m e b) -> Parser i m e b
  (Parser p) >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance (Monoid (m (ParseError i e)), Alternative m) => Alternative (Parser i m e) where
  empty :: Parser i m e a
  empty = Parser $ const $ Left $ pure Empty

  (<|>) :: Parser i m e a -> Parser i m e a -> Parser i m e a
  (Parser l) <|> (Parser r) = Parser $ \input ->
    case l input of
      Left err_l ->
        case r input of
          Left err_r -> Left $ err_l <|> err_r
          Right (output_r, rest_r) -> Right (output_r, rest_r)
      Right (output_l, rest_l) -> Right (output_l, rest_l)

---

satisfy :: (i -> Bool) -> Parser i [] e i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    x : rest
      | predicate x -> Right (x, rest)
      | otherwise -> Left [Unexpected x]

char :: Eq i => i -> Parser i [] e i
char i = satisfy (== i)

string :: Eq i => [i] -> Parser i [] e [i]
string = traverse char
