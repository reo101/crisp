module Crisp.ReadEvalPrintLoop where

import Crisp.Interpreter (Environment (..), eval)
import Crisp.Parser (crisp)
import ParserCombinators.Datatypes (Offset (..), Parser (..))
import System.Console.Haskeline (InputT, getInputLine, outputStrLn)
import Control.Monad.State (StateT(..))

repl :: Offset -> Environment -> InputT IO ()
repl offset env = do
  Just input <- getInputLine "crisp >>= "

  let parsedInput = parse crisp input offset

  case parsedInput of
    Left err -> do
      -- Parser couln't parse the input
      outputStrLn (show err)

      -- Repeat the REPL with the same environment
      repl offset env
    Right (offset', code, "") -> do
      -- Evaluate the input in the current environment
      (result, env') <- runStateT (eval code) env

      -- Print the result of the evaluation
      outputStrLn (show result)

      -- Repeat the REPL with the updated environment
      repl offset' env'
    Right (_, _, _) -> do
      -- Parser couln't parse the whole input - too much (unparsable) code
      outputStrLn "Too much code"

      -- Repeat the REPL with the same environment
      repl offset env
