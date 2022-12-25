{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}

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
    error "Unimplemented" bindings body
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
      fromList
        [ (s, (cr, env))
        | s <- fArgs
        | cr <- args
        ]
        <> fKnownArgs

    envNew :: Environment
    envNew =
      Environment
        { eBindings = (uncurry $ flip eval) <$> fKnownArgsNew
        , eParent = Just fEnv
        }
runF _ _ _ = error "Not a function"

---
