module Main where

import Crisp.Interpreter (Environment(..))
import Data.Map (fromList)
import System.Console.Haskeline (runInputT, defaultSettings)
import Crisp.ReadEvalPrintLoop (repl)

main :: IO ()
main = do
  let offset = 0
  let env = Environment { eBindings = fromList [], eParent = Nothing }
  runInputT defaultSettings $ repl offset env
