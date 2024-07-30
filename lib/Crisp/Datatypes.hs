{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}

module Crisp.Datatypes (
  Atom (..),
  Crisp (..),
)
where

---------------------
---- Crisp Types ----
---------------------

data Atom where
  AInteger :: Integer -> Atom
  ABool :: Bool -> Atom
  ASymbol :: String -> Atom
  deriving stock (Show)

---------------
---- Crisp ----
---------------

data Crisp where
  CrAtom :: Atom -> Crisp
  CrSExpr :: [Crisp] -> Crisp
  deriving stock (Show)
