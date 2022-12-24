module Crisp.Parser (
  atom,
  sexpr,
  crisp,
) where

import Data.Foldable (asum)

import Crisp.Datatypes (Atom (..), Crisp (..))
import ParserCombinators.Datatypes (Parser)
import ParserCombinators.Parser (
  between,
  bool,
  integer,
  sepBy1,
  spaces,
  string,
  symbol,
 )

-----------------------
---- Crisp Related ----
-----------------------

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
        (crisp `sepBy1` spaces)

-- -- | Parse a function definition
-- function :: Parser Char [] String Function
-- function = do
--   _ <- string "("
--   spaces
--   _ <- string "fn"
--   spaces
--   name <- optional symbol
--   spaces
--   args <- between (string "[") (string "]") (symbol `sepBy` spaces)
--   spaces
--   body <- crisp `sepBy` spaces
--   spaces
--   _ <- string ")"
--   return $ Function name args body
--
-- -- | Parse a let expression
-- letExpr :: Parser Char [] String LetExpr
-- letExpr = do
--   _ <- string "("
--   spaces
--   _ <- string "let"
--   spaces
--   bindings <- between (string "[") (string "]") (binding `sepBy` spaces)
--   spaces
--   body <- crisp
--   spaces
--   _ <- string ")"
--   return $ LetExpr bindings body
--
-- -- | Parse a let binding
-- binding :: Parser Char [] String Binding
-- binding = do
--   _ <- string "("
--   spaces
--   name <- symbol
--   spaces
--   value <- crisp
--   spaces
--   _ <- string ")"
--   return (name, value)

-- | Parse a whole crisp expression
crisp :: Parser String [] String Crisp
crisp =
  asum
    -- [ CrFunction <$> function
    -- , CrLetExpr <$> letExpr
    -- , CrAtom <$> atom
    -- , CrSExpr <$> sexpr
    -- ]
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
