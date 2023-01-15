module Crisp.Parser (
  atom,
  sexpr,
  crisp,
) where

import Data.Foldable (asum)

import Crisp.Datatypes (Atom (..), Crisp (..))
import ParserCombinators.Datatypes (Parser (..))
import ParserCombinators.Parser (
  between,
  bool,
  integer,
  sepBy,
  spaces,
  string,
  symbol,
 )

-- | Parse an atom
atom :: Parser String [] String Atom
atom =
  asum
    [ AInteger <$> integer
    , ABool <$> bool
    , ASymbol <$> symbol
    ]

-- | Parse a S expression
sexpr :: Parser String [] String [Crisp]
sexpr =
  asum $
    sexprWith
      <$> [ ("(", ")")
          , ("[", "]")
          , ("{", "}")
          , ("<", ">")
          ]
  where
    sexprWith (left, right) =
      between
        (string left <* spaces)
        (spaces *> string right)
        (crisp `sepBy` spaces)

-- | Parse a whole crisp expression
crisp :: Parser String [] String Crisp
crisp =
  asum
    [ CrAtom <$> atom
    , CrSExpr <$> sexpr
    ]
