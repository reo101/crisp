{-# LANGUAGE TypeApplications #-}

module Crisp.ReadEvalPrintLoop where

-- import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (StateT (..))
import Crisp.Interpreter (Environment (..), eval)
import Crisp.Parser (crisp)
import ParserCombinators.Datatypes (Offset (..), Parser (..))
import System.Console.Haskeline (InputT, getInputLine, outputStrLn)

repl :: Offset -> Environment -> InputT IO ()
repl offset env = do
  Just input <- getInputLine "crisp >>= "

  let parsedInput = parse crisp input offset

  case parsedInput of
    Left err -> do
      -- Parser couldn't parse the input
      outputStrLn (show err)

      -- Repeat the REPL with the same environment
      repl offset env
    Right (offset', code, "") -> do
      -- Evaluate the input in the current environment

      let (result, env') =
            runIdentity $
                (flip runStateT) env $
                  eval @(StateT Environment Identity) code

      outputStrLn (show result)

      repl offset' env'

      --- Preferred implementation but loops on recursive `let`s
      -- let takovata =
      --       runIdentity $
      --         runExceptT $
      --           (flip runStateT) env $
      --             eval @(StateT Environment (ExceptT String Identity)) code
      --
      -- case takovata of
      --   Left s -> do
      --     -- Interpretator couldn't interpret the code
      --     outputStrLn s
      --
      --     -- Repeat the REPL with the same environment
      --     repl offset env
      --   Right (result, env') -> do
      --     -- Print the result of the evaluation
      --     outputStrLn (show result)
      --
      --     -- Repeat the REPL with the updated environment
      --     repl offset' env'
    Right (_, _, _) -> do
      -- Parser couln't parse the whole input - too much (unparsable) code
      outputStrLn "Too much code"

      -- Repeat the REPL with the same environment
      repl offset env
