{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}

module Crisp.Interpreter (
  Val (..),
  Environment (..),
  fetch,
  sed,
  eval,
  runF,
) where

import Control.Applicative (Alternative ((<|>)))

-- import Control.Monad.State (StateT)

import Crisp.Datatypes (Atom (..), Crisp (..))
import Data.Map (Map, fromList, (!?))
import Data.Maybe (fromMaybe)
import Control.Arrow (second)

data Val where
  VFunction ::
    { fEnv :: Environment
    , fArgs :: [String]
    , fKnownArgs :: Map String (Crisp, Environment)
    , fBody :: Crisp
    } ->
    Val
  VBool :: Bool -> Val
  VInteger :: Integer -> Val
  VCons ::
    { car :: Val
    , cdr :: Val
    } ->
    Val
  VEmptyTuple :: () -> Val
  VBuiltin :: Builtin -> Val
  deriving (Show)

data Builtin where
  Cons :: Builtin
  Car :: Builtin
  Cdr :: Builtin
  Plus :: Builtin
  Equals :: Builtin
  deriving (Show)

data Environment where
  Environment ::
    { eBindings :: Map String Val
    , eParent :: Maybe Environment
    } ->
    Environment
  deriving (Show)

fetch :: Environment -> String -> Maybe Val
fetch env s = eBindings env !? s <|> ((eParent env) >>= (\e -> fetch e s))

-- type CrispState = StateT Environment IO ()

sed :: [(String, Crisp)] -> Crisp -> Crisp
sed bindings (CrSExpr crs) = CrSExpr $ sed bindings <$> crs
sed bindings o@(CrAtom (ASymbol os)) = fromMaybe o $ fromList bindings !? os
sed _ cr = cr

eval :: Environment -> Crisp -> Val
eval env expr = case expr of
  CrAtom (ASymbol s) ->
    case s of
      "cons" -> VBuiltin Cons
      "car" -> VBuiltin Car
      "cdr" -> VBuiltin Cdr
      "=" -> VBuiltin Equals
      "+" -> VBuiltin Plus
      _ ->
        fromMaybe (error $ "Undefined symbol " ++ show s ++ " from " ++ show env) $
          fetch env s
  CrAtom (ABool b) ->
    VBool b
  CrAtom (AInteger i) ->
    VInteger i
  CrSExpr [] ->
    VEmptyTuple ()
  CrSExpr [CrAtom (ASymbol "if"), p, t, f] ->
    case eval env p of
      VBool b ->
        eval env $ if b then t else f
      _ ->
        error "Non-boolean in if condition"
  CrSExpr [CrAtom (ASymbol "let"), CrSExpr bindings, body] ->
    eval newEnv body
    where
      checkedBindings :: [(String, Crisp)]
      checkedBindings =
        fromMaybe (error "Bad bindings") $
          sequence $
            fmap
              ( \binding -> case binding of
                  (CrSExpr [CrAtom (ASymbol s), b]) -> Just (s, b)
                  _ -> error ("Bad binding: " ++ show binding)
              )
              bindings
      newEnv =
        Environment
          { eBindings = fromList $ (second $ eval newEnv) <$> checkedBindings
          , eParent = Just env
          }
  CrSExpr [CrAtom (ASymbol "fn"), CrSExpr args, body] ->
    VFunction
      { fEnv = env
      , fArgs = checkedArgs
      , fKnownArgs = fromList []
      , fBody = body
      }
    where
      checkedArgs :: [String]
      checkedArgs =
        fromMaybe (error "Nonsybolic args") $
          sequence $
            fmap
              ( \arg -> case arg of
                  (CrAtom (ASymbol s)) -> Just s
                  _ -> Nothing
              )
              args
  CrSExpr (f : args) ->
    runF env (eval env f) args

runF :: Environment -> Val -> [Crisp] -> Val
runF env VFunction {fEnv, fArgs, fKnownArgs, fBody} args
  | argsN < fArgsN =
      VFunction
        { fEnv = fEnv
        , fArgs = drop argsN fArgs
        , fKnownArgs = fKnownArgsNew
        , fBody = fBody
        }
  | argsN == fArgsN =
      eval envNew fBody
  | otherwise =
      error "Too much args"
  where
    argsN = length args
    fArgsN = length fArgs

    fKnownArgsNew :: Map String (Crisp, Environment)
    fKnownArgsNew =
      -- NOTE: order of (<>)
      --  -> `(fn [a a] a)`
      --  -> right `a` takes precedence
      fromList (zipWith (\s cr -> (s, (cr, env))) fArgs args)
        <> fKnownArgs

    envNew :: Environment
    envNew =
      Environment
        { eBindings = (uncurry $ flip eval) <$> fKnownArgsNew
        , eParent = Just fEnv
        }
runF env (VBuiltin b) args = case b of
  Cons ->
    case args of
      [x, xs] -> VCons {car = eval env x, cdr = eval env xs}
      _ -> error "Cons needs exactly 2 args"
  Car ->
    case args of
      [p] -> case eval env p of
        VCons x _ -> x
        _ -> error ("Expected cons pair, got " ++ show p)
      _ -> error "Car needs exactly 1 arg"
  Cdr ->
    case args of
      [p] -> case eval env p of
        VCons _ xs -> xs
        _ -> error ("Expected cons pair, got " ++ show p)
      _ -> error "Cdr needs exactly 1 arg"
  Plus ->
    case args of
      [x, y] -> case (eval env x, eval env y) of
        (VInteger xi, VInteger yi) -> VInteger $ xi + yi
        _ -> error "Cannot add nonintegers"
      _ -> error "+ expects exactly 2 args"
  Equals ->
    case args of
      [x, y] -> case (eval env x, eval env y) of
        (VInteger xi, VInteger yi) -> VBool $ xi == yi
        (VEmptyTuple (), VEmptyTuple ()) -> VBool True
        (_, VEmptyTuple ()) -> VBool False
        (VEmptyTuple (), _) -> VBool False
        _ -> error "Cannot compare nonintegers"
      _ -> error "= expects exactly 2 args"
runF _ nz _ = error ("Not a function: " ++ show nz)

---
