{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ParserCombinators.Datatypes where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus, ap, liftM)
import Data.Kind (Type)

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
  deriving stock (Eq)

instance (Show i, Show e) => Show (ParseErrorType i e) where
  show :: ParseErrorType i e -> String
  show EndOfInput = "End of Input"
  show (Unexpected a) = "Unexpected " ++ show a
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

instance Functor (Parser s m e) where
  fmap :: (a -> b) -> Parser s m e a -> Parser s m e b
  fmap = liftM

instance Applicative (Parser s m e) where
  pure :: a -> Parser s m e a
  pure a = Parser $ \input offset -> Right (offset, a, input)

  (<*>) :: Parser s m e (a -> b) -> Parser s m e a -> Parser s m e b
  (<*>) = ap

instance Monad (Parser s m e) where
  (>>=) :: Parser s m e a -> (a -> Parser s m e b) -> Parser s m e b
  Parser p >>= k = Parser $ \input offset -> do
    (offset', output, rest) <- p input offset
    parse (k output) rest offset'

instance (Alternative m) => Alternative (Parser s m e) where
  empty :: Parser s m e a
  empty = Parser $ \_ offset -> Left $ pure $ ParseError offset Empty

  (<|>) :: Parser s m e a -> Parser s m e a -> Parser s m e a
  (Parser l) <|> (Parser r) = Parser $ \input offset ->
    case (l input offset, r input offset) of
      (Right res_l, _) -> Right res_l
      (_, Right res_r) -> Right res_r
      (Left err_l, Left err_r) -> Left $ err_l <|> err_r

instance (Alternative m) => MonadPlus (Parser s m e)
