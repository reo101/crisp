module Crisp.ReadEvalPrintLoop where

import Crisp.Datatypes (Atom (ASymbol), Crisp (..))
import Crisp.Interpreter (Environment (..), Val (VEmptyTuple), eval)
import Crisp.Parser (crisp)
import Data.Map (fromList)
import ParserCombinators.Datatypes (Offset (..), Parser (..))
import System.Console.Haskeline (InputT, getInputLine, outputStrLn)

repl :: Offset -> Environment -> InputT IO ()
repl offset env = do
  Just input <- getInputLine "crisp >>= "

  let (env', offset', effect) =
        case parse crisp input offset of
          Left err ->
            (env, offset, outputStrLn $ show err)
          Right (offset'', code, "") ->
            case eval' env code of
              (env'', val) -> (env'', offset'', outputStrLn $ show val)
          Right (_, _, _) ->
            (env, offset, outputStrLn $ show "Too much code")

  effect

  repl offset' env'

eval' :: Environment -> Crisp -> (Environment, Val)
eval' env code =
  case code of
    CrSExpr [CrAtom (ASymbol "define"), CrAtom (ASymbol s), body] ->
      -- (Environment {eBindings=fromList [(s, eval env body)], eParent=Just env}, VEmptyTuple ())
      ( env {eBindings = fromList [(s, eval env body)] <> eBindings env}
      , VEmptyTuple ()
      )
    _ -> (env, eval env code)

-- -- The Read Eval Print Loop itself.
-- loop :: Environment -> IO ()
-- loop env =
--   do putStr "> "
--      hFlush stdout
--      line <- getLine
--      if line == "quit"
--        then return ()
--        else do
--          let parser = analyzeExpressionSequence . parseSequence . tokenize
--              (newEnv, result) = (evalSequence env (parser line))
--          putStrLn $ show result
--          hFlush stdout
--          loop newEnv
--
-- readEvalPrintLoop :: IO ()
-- readEvalPrintLoop = do env <- loadCoreLibrary
--                        putStr welcomeMessage
--                        loop env
