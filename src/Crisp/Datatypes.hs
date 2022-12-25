{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}

module Crisp.Datatypes where

---------------------
---- Crisp Types ----
---------------------

data Atom where
  AInteger :: Integer -> Atom
  ABool :: Bool -> Atom
  ASymbol :: String -> Atom
  deriving (Show)

---------------
---- Crisp ----
---------------

data Crisp where
  CrAtom :: Atom -> Crisp
  CrSExpr :: [Crisp] -> Crisp
  deriving (Show)
