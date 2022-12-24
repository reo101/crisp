{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module ParserCombinators.Datatypes where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus, ap, liftM)
import Data.Kind (Type)

------------------------
---- ParseErrorType ----
------------------------

data ParseErrorType i e
  = EndOfInput
  | Expected i i
  | ExpectedEndOfFile i
  | CustomError e
  | Empty
  deriving (Eq)

instance (Show i, Show e) => Show (ParseErrorType i e) where
  show :: ParseErrorType i e -> String
  show EndOfInput = "End of Input"
  show (Expected e a) = "Expected " ++ show e ++ ", got " ++ show a
  show (ExpectedEndOfFile a) = "Expected EndOfFile, got " ++ show a
  show (CustomError e) = "Custom Error: " ++ show e
  show Empty = "No error"

----------------
---- Offset ----
----------------

newtype Offset where
  Offset :: Integer -> Offset
  deriving newtype (Num, Enum)

instance Show Offset where
  show :: Offset -> String
  show (Offset o) = "offset " ++ show o

--------------------
---- ParseError ----
--------------------

data ParseError i e where
  ParseError ::
    { errOffset :: Offset
    , errError :: ParseErrorType i e
    } ->
    ParseError i e

instance (Show i, Show e) => Show (ParseError i e) where
  show :: ParseError i e -> String
  show (ParseError o e) = "Error: " ++ show e ++ ", at " ++ show o

----------------
---- Parser ----
----------------

newtype Parser stream m e a where
  Parser ::
    { parse ::
        stream ->
        Offset ->
        Either
          (m (ParseError (Token stream) e))
          (Offset, a, stream)
    } ->
    Parser stream m e a

type Token :: Type -> Type
type family Token stream

-- type instance Token Text = Char
-- type instance Token ByteString = Word8
type instance Token [a] = a

instance Functor (Parser i m e) where
  fmap :: (a -> b) -> Parser i m e a -> Parser i m e b
  fmap = liftM

instance Applicative (Parser i m e) where
  pure :: a -> Parser i m e a
  pure = return

  (<*>) :: Parser i m e (a -> b) -> Parser i m e a -> Parser i m e b
  (<*>) = ap

instance Monad (Parser i m e) where
  return :: a -> Parser i m e a
  return a = Parser $ \input offset -> Right (offset, a, input)

  (>>=) :: Parser i m e a -> (a -> Parser i m e b) -> Parser i m e b
  Parser p >>= k = Parser $ \input offset -> do
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
