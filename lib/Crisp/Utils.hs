{-# OPTIONS_GHC -Wno-orphans #-}

module Crisp.Utils (
  expected,
  firstA,
  lineNumberLens,
  offsetLens,
  parse,
  sepBy1,
  throwErrorToIOFinal,
  type TParser,
  type TParseError,
) where

import Control.Exception (Exception, throwIO)
import Control.Lens (
  Lens',
  iso,
  to,
  view,
 )
import Data.Data (Proxy (Proxy))
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Void (Void)
import Polysemy (
  Final,
  Member,
  Sem,
  embedFinal,
 )
import Polysemy.Error (Error, errorToIOFinal, throw)
import Text.Megaparsec (ParseErrorBundle (bundleErrors))
import Text.Megaparsec qualified as MP

-- Define the type alias for your parser
type TParser t a = MP.Parsec Void t a

-- | A megaparsec error type
type TParseError t = MP.ParseErrorBundle t Void

firstA :: (Applicative m) => (a -> m b) -> Either a b -> m b
firstA f = either f pure

parse
  :: (Member (Error String) r, MP.VisualStream t)
  => TParser t a
  -- ^ Parser
  -> String
  -- ^ File path
  -> t
  -- ^ Content
  -> Sem r a
parse parser file code = firstA (throw . (MP.parseErrorPretty =<<) . (view $ #bundleErrors . to NE.toList)) $ MP.parse parser file code

instance (Ord a, Show a) => MP.VisualStream [a] where
  showTokens :: Proxy [a] -> NonEmpty (MP.Token [a]) -> String
  showTokens Proxy t = show =<< NE.toList t

instance Exception String

throwErrorToIOFinal
  :: forall e r b
   . (Exception e)
  => (Member (Final IO) r)
  => Sem (Error e : r) b
  -> Sem r b
throwErrorToIOFinal = (firstA (embedFinal . throwIO) =<<) . errorToIOFinal

-- | Parse a non-empty list of items separated by the specified separator parser
sepBy1 :: (MP.Stream t) => TParser t a -> TParser t b -> TParser t (NonEmpty a)
sepBy1 p sep = do
  x <- p
  xs <- MP.many (sep *> p)
  return $ x :| xs

lineNumberLens :: Lens' (MP.State s e) Int
lineNumberLens = #statePosState . #pstateSourcePos . #sourceLine . iso MP.unPos MP.mkPos

offsetLens :: Lens' (MP.State s e) Int
offsetLens = #stateOffset

expected :: e -> Set (MP.ErrorItem e)
expected = Set.singleton . MP.Tokens . pure
