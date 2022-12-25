module Crisp.Parser (
  atom,
  sexpr,
  crisp,
) where

import Data.Foldable (asum)

import Crisp.Datatypes (Atom (..), Crisp (..))
import Data.List.NonEmpty (toList)
import ParserCombinators.Datatypes (Parser (..))
import ParserCombinators.Parser (
  between,
  bool,
  integer,
  sepBy1,
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
        (string left)
        (string right)
        (toList <$> (crisp `sepBy1` spaces))

-- | Parse a whole crisp expression
crisp :: Parser String [] String Crisp
crisp =
  asum
    [ CrAtom <$> atom
    , CrSExpr <$> sexpr
    ]

-- >>> parse crisp "(let [(add (fn [x y] 5))] (add 5 6))" 0
-- Right
--   ( 36
--   , CrSExpr
--       [ CrAtom
--           ( ASymbol
--               "let"
--           )
--       , CrSExpr
--           [ CrSExpr
--               [ CrAtom
--                   ( ASymbol
--                       "add"
--                   )
--               , CrSExpr
--                   [ CrAtom
--                       ( ASymbol
--                           "fn"
--                       )
--                   , CrSExpr
--                       [ CrAtom
--                           ( ASymbol
--                               "x"
--                           )
--                       , CrAtom
--                           ( ASymbol
--                               "y"
--                           )
--                       ]
--                   , CrAtom
--                       ( AInteger
--                           5
--                       )
--                   ]
--               ]
--           ]
--       , CrSExpr
--           [ CrAtom
--               ( ASymbol
--                   "add"
--               )
--           , CrAtom
--               ( AInteger
--                   5
--               )
--           , CrAtom
--               ( AInteger
--                   6
--               )
--           ]
--       ]
--   , ""
--   )

-- kek :: Either a (Integer, Crisp, String)
-- kek =
--   Right
--     ( 36
--     , CrLetExpr
--         ( LetExpr
--             { lBindings =
--                 [
--                   ( "add"
--                   , CrFunction
--                       ( Function
--                           { fName = Nothing
--                           , fArgs =
--                               [ "x"
--                               , "y"
--                               ]
--                           , fBody =
--                               [ CrAtom
--                                   (AInteger 5)
--                               ]
--                           }
--                       )
--                   )
--                 ]
--             , lBody =
--                 CrSExpr
--                   [ CrAtom
--                       (ASymbol "add")
--                   , CrAtom
--                       (AInteger 5)
--                   , CrAtom
--                       (AInteger 6)
--                   ]
--             }
--         )
--     , ""
--     )
