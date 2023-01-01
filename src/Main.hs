module Main where

import Crisp.Interpreter (emptyEnv)
import Crisp.ReadEvalPrintLoop (repl)
import System.Console.Haskeline (defaultSettings, runInputT)

main :: IO ()
main = do
  runInputT defaultSettings $ repl 0 emptyEnv
