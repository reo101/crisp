{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module ParserCombinators.Datatypes where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)

------------------------
---- ParseErrorType ----
------------------------

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

--------------------
---- ParseError ----
--------------------

type Offset = Integer

data ParseError i e where
  ParseError
    :: { errOffset :: Offset
       , errError :: ParseErrorType i e
       }
    -> ParseError i e


instance (Show i, Show e) => Show (ParseError i e) where
  show :: ParseError i e -> String
  show (ParseError o e) = "Error: " ++ show e ++ ", at offset " ++ show o

----------------
---- Parser ----
----------------

newtype Parser i m e a where
  Parser
    :: { parse
          :: [i]
          -> Offset
          -> Either
              (m (ParseError i e))
              (Offset, a, [i])
       }
    -> Parser i m e a

instance Functor (Parser i m e) where
  fmap :: (a -> b) -> Parser i m e a -> Parser i m e b
  fmap f (Parser p) = Parser $ \input offset -> do
    (offset', output, rest) <- p input offset
    pure (offset', f output, rest)

instance Applicative (Parser i m e) where
  pure :: a -> Parser i m e a
  pure a = Parser $ \input offset -> Right (offset, a, input)

  (<*>) :: Parser i m e (a -> b) -> Parser i m e a -> Parser i m e b
  Parser f <*> Parser p = Parser $ \input offset -> do
    (offset', f', rest) <- f input offset
    (offset'', output, rest') <- p rest offset'
    pure (offset'', f' output, rest')

instance Monad (Parser i m e) where
  (>>=) :: Parser i m e a -> (a -> Parser i m e b) -> Parser i m e b
  (Parser p) >>= k = Parser $ \input offset -> do
    (offset', output, rest) <- p input offset
    parse (k output) rest offset'

instance (Alternative m) => Alternative (Parser i m e) where
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

instance (Alternative m) => MonadPlus (Parser i m e)
