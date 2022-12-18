{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}

module Parser
  ( ParseError (..),
    Parser (..),
    satisfy,
    char,
    string,
  )
where

import Control.Applicative (Alternative (..))

-- newtype Parser' i e a = Parser'
--   { runParser' :: [i] -> Either [ParseError i e] ([i], a)
--   }
--   deriving stock (Generic1)
--   deriving
--     (Functor, Applicative)
--     via ((,) [i] `Compose` Either [ParseError i e] `Compose` (->) [i])

data ParseError i e
  = EndOfInput
  | Unexpected i
  | CustomError e
  | Empty
  deriving (Eq, Show)

newtype Parser i e a where
  Parser ::
    {runParser :: [i] -> Either [ParseError i e] (a, [i])} ->
    Parser i e a

instance Functor (Parser i e) where
  fmap :: (a -> b) -> Parser i e a -> Parser i e b
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative (Parser i e) where
  pure :: a -> Parser i e a
  pure a = Parser $ \input -> Right (a, input)

  (<*>) :: Parser i e (a -> b) -> Parser i e a -> Parser i e b
  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad (Parser i e) where
  (>>=) :: Parser i e a -> (a -> Parser i e b) -> Parser i e b
  (Parser p) >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance Alternative (Parser i e) where
  empty :: Parser i e a
  empty = Parser $ const $ Left [Empty]

  (<|>) :: Parser i e a -> Parser i e a -> Parser i e a
  (Parser l) <|> (Parser r) = Parser $ \input ->
    case l input of
      Left err_l ->
        case r input of
          Left err_r -> Left $ err_l <> err_r
          Right (output_r, rest_r) -> Right (output_r, rest_r)
      Right (output_l, rest_l) -> Right (output_l, rest_l)

---

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    x : rest
      | predicate x -> Right (x, rest)
      | otherwise -> Left [Unexpected x]

char :: Eq i => i -> Parser i e i
char i = satisfy (== i)

string :: Eq i => [i] -> Parser i e [i]
string = traverse char
