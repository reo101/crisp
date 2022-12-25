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

data Val where
  VFunction ::
    { fEnv :: Environment
    , fArgs :: [String]
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
    { eBindings :: Map String Crisp
    , eParent :: Maybe Environment
    } ->
    Environment
  deriving (Show)

fetch :: Environment -> String -> Maybe Crisp
fetch env s = eBindings env !? s <|> ((eParent env) >>= (\e -> fetch e s))

-- type CrispState = StateT Environment IO ()

sed :: [(String, Crisp)] -> Crisp -> Crisp
sed bindings (CrSExpr crs) = CrSExpr $ sed bindings <$> crs
sed bindings o@(CrAtom (ASymbol os)) = case fromList bindings !? os of
  Nothing -> o
  Just ns -> ns
sed _ cr = cr

eval :: Environment -> Crisp -> (Val, Environment)
eval env expr = case expr of
  CrAtom (ASymbol s) ->
    eval env $ fromMaybe (error "Undefined symbol") $ fetch env s
  CrAtom (ABool b) ->
    (VBool b, env)
  CrAtom (AInteger i) ->
    (VInteger i, env)
  CrSExpr [] ->
    (VEmptyTuple (), env)
  CrSExpr [CrAtom (ASymbol "if"), p, t, f] ->
    case eval env p of
      (VBool b, _) ->
        if b
          then eval env t
          else eval env f
      _ ->
        error "Non-boolean in if condition"
  CrSExpr [CrAtom (ASymbol "let"), CrSExpr bindings, body] ->
    error "Unimplemented" bindings body
  CrSExpr [CrAtom (ASymbol "fn"), CrSExpr args, body] ->
    ( VFunction
        { fEnv = env
        , fArgs = checkedArgs
        , fBody = body
        }
    , env
    )
    where
      checkedArgs :: [String]
      checkedArgs =
        fromMaybe (error "Nonsybolic args") $
          sequence $
            map
              ( \arg -> case arg of
                  (CrAtom (ASymbol s)) -> Just s
                  _ -> Nothing
              )
              args
  CrSExpr (f : args) ->
    (runF (fst $ eval env f) args, env)

runF :: Val -> [Crisp] -> Val
runF VFunction {fEnv, fArgs, fBody} args
  | argsN < fArgsN =
      VFunction
        { fEnv = fEnv
        , fArgs = drop argsN fArgs
        , fBody = sed (zip fArgs args) fBody
        }
  | argsN == fArgsN =
      fst $ eval fEnv $ sed (zip fArgs args) fBody
  | otherwise =
      error "Too much args"
  where
    argsN = length args
    fArgsN = length fArgs
runF _ _ = error "Not a function"

---
