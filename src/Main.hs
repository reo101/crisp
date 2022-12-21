module Main where

import ParserCombinators.Datatypes (Parser (parse))
import Crisp.Parser (crisp)

main :: IO ()
main = do
  input <- getLine
  let result = parse crisp input 0
  print result
