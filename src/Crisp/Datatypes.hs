{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}

module Crisp.Datatypes where

---------------------
---- Crisp Types ----
---------------------

-- data Function where
--   Function
--     :: { fName :: Maybe String
--        , fArgs :: [String]
--        , fBody :: [Crisp]
--        }
--     -> Function
--   deriving (Show)
--
-- type Binding = (String, Crisp)
--
-- data LetExpr where
--   LetExpr
--     :: { lBindings :: [Binding]
--        , lBody :: Crisp
--        } -> LetExpr
--   deriving (Show)

data Atom where
  AInteger :: Integer -> Atom
  ABool :: Bool -> Atom
  ASymbol :: String -> Atom
  deriving (Show)

---------------
---- Crisp ----
---------------

data Crisp where
  -- CrFunction :: Function -> Crisp
  -- CrLetExpr :: LetExpr -> Crisp
  CrAtom :: Atom -> Crisp
  CrSExpr :: [Crisp] -> Crisp
  deriving (Show)
