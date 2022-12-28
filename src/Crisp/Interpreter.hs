{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Crisp.Interpreter (
  Val (..),
  Environment (..),
  fetch,
  sed,
  eval,
  runF,
) where

import Control.Applicative (Alternative ((<|>)))

import Control.Monad.State (MonadState (..), evalStateT, modify, MonadFix)
import Crisp.Datatypes (Atom (..), Crisp (..))
import Data.Either (partitionEithers)
import Data.Kind (Type)
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
  VBuiltin :: Builtin -> Val
  deriving (Show)

data Builtin where
  Define :: Builtin
  Equals :: Builtin
  Plus :: Builtin
  Cons :: Builtin
  Car :: Builtin
  Cdr :: Builtin
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

sed :: [(String, Crisp)] -> Crisp -> Crisp
sed bindings (CrSExpr crs) = CrSExpr $ sed bindings <$> crs
sed bindings o@(CrAtom (ASymbol os)) = fromMaybe o $ fromList bindings !? os
sed _ cr = cr

eval ::
  forall (m :: Type -> Type).
  (MonadState Environment m, MonadFail m, MonadFix m) =>
  Crisp ->
  m Val
eval expr = do
  env <- get

  case expr of
    CrAtom (ASymbol s) -> do
      either fail return $
        case s of
          "define" -> Right $ VBuiltin Define
          "cons" -> Right $ VBuiltin Cons
          "car" -> Right $ VBuiltin Car
          "cdr" -> Right $ VBuiltin Cdr
          "=" -> Right $ VBuiltin Equals
          "+" -> Right $ VBuiltin Plus
          _ ->
            fromMaybe (Left $ "Undefined symbol " ++ show s ++ " from " ++ show env) $
              Right <$> fetch env s
    CrAtom (ABool b) -> do
      return $ VBool b
    CrAtom (AInteger i) -> do
      return $ VInteger i
    CrSExpr [] -> do
      return $ VEmptyTuple ()
    CrSExpr [CrAtom (ASymbol "if"), p, t, f] -> do
      predicate <- eval p
      case predicate of
        VBool b -> do
          eval $ if b then t else f
        _ ->
          fail "Non-boolean in if condition"
    CrSExpr [CrAtom (ASymbol "let"), CrSExpr bindings, body] -> mdo
      checkedBindings <- genCheckedBindings newEnv

      let newEnv =
            Environment
              { eBindings = fromList $ checkedBindings
              , eParent = Just env
              }

      evalStateT (eval body) newEnv
      where
        genCheckedBindings :: Environment -> m [(String, Val)]
        genCheckedBindings newEnv = do
          oldState <- get
          put newEnv
          let eitherParserBindings =
                fmap
                  ( \binding -> case binding of
                      (CrSExpr [CrAtom (ASymbol s), b]) -> Right $ (s, b)
                      _ -> Left $ ("Bad binding: " ++ show binding)
                  )
                  bindings
          parsedBindings <- accumulateEithers $ eitherParserBindings
          evaluatedBindings <- mapM (\(s, b) -> (s,) <$> eval b) parsedBindings
          put oldState
          return evaluatedBindings
    CrSExpr [CrAtom (ASymbol "fn"), CrSExpr args, body] -> do
      checkedArgs <- genCheckedArgs

      return $
        VFunction
          { fEnv = env
          , fArgs = checkedArgs
          , fKnownArgs = fromList []
          , fBody = body
          }
      where
        genCheckedArgs :: m [String]
        genCheckedArgs =
            accumulateEithers $
              fmap
                ( \arg -> case arg of
                    (CrAtom (ASymbol s)) -> Right $ s
                    _ -> Left $ fail ("Nonsybolic binding: " ++ show arg)
                )
                args
    CrSExpr (fCode : args) -> do
      f <- eval fCode
      runF f args
  where
    accumulateEithers :: [Either String r] -> m [r]
    accumulateEithers es = do
      let (errors, results) = partitionEithers es
      case errors of
        [] -> return results
        (err:_) -> fail err

runF ::
  forall (m :: Type -> Type).
  (MonadState Environment m, MonadFail m, MonadFix m) =>
  Val ->
  [Crisp] ->
  m Val
runF VFunction {fEnv, fArgs, fKnownArgs, fBody} args = do
  env <- get

  if
      | argsN < fArgsN -> do
          return $
            VFunction
              { fEnv = fEnv
              , fArgs = drop argsN fArgs
              , fKnownArgs = fKnownArgsNew env
              , fBody = fBody
              }
      | argsN == fArgsN -> do
          newEnv <- genNewEnv
          evalStateT (eval fBody) newEnv
      | otherwise -> do
          fail "Too much args"
  where
    argsN = length args
    fArgsN = length fArgs

    fKnownArgsNew :: Environment -> Map String (Crisp, Environment)
    fKnownArgsNew env =
      -- NOTE: order of (<>)
      --  -> `(fn [a a] a)`
      --  -> right `a` takes precedence
      fromList (zipWith (\s cr -> (s, (cr, env))) fArgs args)
        <> fKnownArgs

    genNewEnv :: m Environment
    genNewEnv = do
      env <- get

      newBindings <-
        sequence $
          (\(cr, env') -> evalStateT (eval cr) env')
            <$> fKnownArgsNew env

      return $
        Environment
          { eBindings = newBindings
          , eParent = Just fEnv
          }
runF (VBuiltin b) args = do
  case b of
    Define ->
      case args of
        [sCode, bodyCode] -> do
          either err id $ case sCode of
            CrAtom (ASymbol s) -> Right $ mdo
              modify $
                ( \st ->
                    st
                      { eBindings = fromList [(s, body)]
                      , eParent = Just st
                      }
                )
              body <- eval bodyCode
              return $ VEmptyTuple ()
            _ -> Left $ "define expects a symbol for its first argument"
        _ -> err "define expects 2 args"
    Equals ->
      case args of
        [xCode, yCode] -> do
          x <- eval xCode
          y <- eval yCode
          handleError $ case (x, y) of
            (VInteger xi, VInteger yi) -> Right $ VBool $ xi == yi
            (VEmptyTuple (), VEmptyTuple ()) -> Right $ VBool True
            (_, VEmptyTuple ()) -> Right $ VBool False
            (VEmptyTuple (), _) -> Right $ VBool False
            _ -> Left $ "Cannot compare nonintegers"
        _ -> err "= expects exactly 2 args"
    Plus ->
      case args of
        [xCode, yCode] -> do
          x <- eval xCode
          y <- eval yCode
          handleError $ case (x, y) of
            (VInteger xi, VInteger yi) -> Right $ VInteger $ xi + yi
            _ -> Left $ "Cannot add nonintegers"
        _ -> err "+ expects exactly 2 args"
    Cons ->
      case args of
        [xCode, xsCode] -> do
          x <- eval xCode
          xs <- eval xsCode
          return $
            VCons
              { car = x
              , cdr = xs
              }
        _ -> err "Cons needs exactly 2 args"
    Car ->
      case args of
        [pCode] -> do
          p <- eval pCode
          handleError $ case p of
            VCons x _ -> Right $ x
            _ -> Left ("Expected cons pair, got " ++ show pCode)
        _ -> err "Car needs exactly 1 arg"
    Cdr ->
      case args of
        [pCode] -> do
          p <- eval pCode
          handleError $ case p of
            VCons _ xs -> Right $ xs
            _ -> Left ("Expected cons pair, got " ++ show pCode)
        _ -> err "Cdr needs exactly 1 arg"
  where
    err :: String -> m a
    err = fail

    handleError :: Either String r -> m r
    handleError = either err return
runF nz _ = fail ("Not a function: " ++ show nz)

---
