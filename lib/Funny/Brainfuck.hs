{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Funny.Brainfuck where

import Control.Lens (Field1 (_1), (%~), (&), (+~), (-~), (.~), (^.))
import Control.Monad (join, void, when)
import Crisp.Utils (parse, throwErrorToIOFinal)
import Data.Char (chr, ord)
import Data.Foldable (traverse_)
import Data.Functor.Foldable (ana)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Polysemy (Member, Members, Sem, embed, embedToFinal, interpret, makeSem, runFinal, runM)
import Polysemy.Error (Error, errorToIOFinal, throw)
import Polysemy.Input (Input, input, runInputList, runInputSem)
import Polysemy.Output (Output, output, runOutputMonoid, runOutputSem)
import Polysemy.State (State, evalState, gets, modify)
import Polysemy.Trace (Trace (Trace), trace, traceToStdout)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, many, runParser, satisfy, sepBy)
import Text.Megaparsec.Char (char)

-- AST definition
data BFCommand
  = BFCMoveRight
  | BFCMoveLeft
  | BFCIncrement
  | BFCDecrement
  | BFCOutput
  | BFCInput
  | BFCLoop [BFCommand]
  deriving stock (Show, Eq)

-- -- Parser
-- parseBrainfuck :: String -> [BFCommand]
-- parseBrainfuck = fst . parseProgram
--   where
--     parseProgram [] = ([], [])
--     parseProgram (c : cs) = case c of
--       '>' -> first (BFCMoveRight :) $ parseProgram cs
--       '<' -> first (BFCMoveLeft :) $ parseProgram cs
--       '+' -> first (BFCIncrement :) $ parseProgram cs
--       '-' -> first (BFCDecrement :) $ parseProgram cs
--       '.' -> first (BFCOutput :) $ parseProgram cs
--       ',' -> first (BFCInput :) $ parseProgram cs
--       '[' ->
--         let (loopBody, rest) = parseProgram cs
--             (remainingProgram, unmatched) = parseProgram rest
--          in (BFCLoop loopBody : remainingProgram, unmatched)
--       ']' -> ([], cs)
--       _ -> parseProgram cs

-- Type alias for the parser
type Parser = Parsec Void Text

-- Parser for Brainfuck commands
commandParser :: Parser [BFCommand]
commandParser = parseCommand `sepBy` ignoreComment

-- Parses individual Brainfuck commands, ignoring comments
parseCommand :: Parser BFCommand
parseCommand =
  choice
    [ BFCMoveRight <$ char '>'
    , BFCMoveLeft <$ char '<'
    , BFCIncrement <$ char '+'
    , BFCDecrement <$ char '-'
    , BFCOutput <$ char '.'
    , BFCInput <$ char ','
    , BFCLoop <$> between (char '[') (char ']') (commandParser)
    ]

-- Ignore any character that is not a valid Brainfuck command
ignoreComment :: Parser ()
ignoreComment = void $ many (satisfy isCommentChar)
  where
    isCommentChar :: Char -> Bool
    isCommentChar c = c `notElem` ['>', '<', '+', '-', '.', '[', ']', ',']

-- Function to parse a Brainfuck program from a string
parseBrainfuck :: (Member (Error String) r) => String -> Sem r [BFCommand]
parseBrainfuck input = parse commandParser "kek" $ T.pack input

-- State
data Stream a = Cons a (Stream a)

makeBaseFunctor ''Stream

repeatStream :: a -> Stream a
repeatStream = ana (join ConsF)

data Tape = Tape
  { left :: Stream Int
  , current :: Int
  , right :: Stream Int
  }
  deriving stock (Generic)

initialTape :: Tape
initialTape = Tape (repeatStream 0) 0 (repeatStream 0)

data BrainfuckEffect m a where
  MoveLeft :: BrainfuckEffect m ()
  MoveRight :: BrainfuckEffect m ()
  Increment :: BrainfuckEffect m ()
  Decrement :: BrainfuckEffect m ()
  OutputChar :: BrainfuckEffect m ()
  InputChar :: BrainfuckEffect m ()
  GetCurrentValue :: BrainfuckEffect m Int
  EndOfInput :: BrainfuckEffect m ()

makeSem ''BrainfuckEffect

-- Interpreter for AST
interpretBF
  :: (Members '[BrainfuckEffect, Trace] r)
  => [BFCommand]
  -> Sem r ()
interpretBF = traverse_ interpret
  where
    interpret cmd = do
      trace $ "Executing command: " ++ show cmd
      case cmd of
        BFCMoveRight -> moveRight
        BFCMoveLeft -> moveLeft
        BFCIncrement -> increment
        BFCDecrement -> decrement
        BFCOutput -> outputChar
        BFCInput -> inputChar
        BFCLoop body -> do
          val <- getCurrentValue
          when (val /= 0) $ do
            interpretBF body
            interpret (BFCLoop body)

-- Main BrainfuckEffect handler
runBrainfuckEffect
  :: (Members '[State Tape, Input (Maybe Char), Output Char, Error String, Trace] r)
  => Sem (BrainfuckEffect ': r) a
  -> Sem r a
runBrainfuckEffect = interpret $ \case
  MoveLeft -> do
    modify $ \tape ->
      let (Cons l ls) = tape ^. #left
       in tape
            & #left .~ ls
            & #right %~ Cons (tape ^. #current)
            & #current .~ l
    trace "Moved left"
  MoveRight -> do
    modify $ \tape ->
      let (Cons r rs) = tape ^. #right
       in tape
            & #left %~ Cons (tape ^. #current)
            & #current .~ r
            & #right .~ rs
    trace "Moved right"
  Increment -> do
    modify $ #current +~ 1
    trace "Incremented current cell"
  Decrement -> do
    modify $ #current -~ 1
    trace "Decremented current cell"
  OutputChar -> do
    val <- gets (^. #current)
    output (chr val)
    trace $ "Output char: " ++ [chr val]
  InputChar -> do
    mc <- input
    case mc of
      Just c -> do
        modify $ #current .~ ord c
        trace $ "Input char: " ++ show c
      Nothing -> do
        error "End of input"
  GetCurrentValue ->
    gets (^. #current)
  EndOfInput ->
    pure ()

-- Ignore traces
traceToNowhere
  :: Sem (Trace ': r) a
  -> Sem r a
traceToNowhere = interpret \case
  Trace _msg -> return ()

runBrainfuckProgram :: String -> IO ()
runBrainfuckProgram program = do
  ast <-
    runFinal
      . throwErrorToIOFinal
      . parseBrainfuck
      $ program
  runFinal
    . embedToFinal
    . throwErrorToIOFinal
    . (if False then traceToStdout else traceToNowhere)
    . runInputSem (pure <$> embed getChar)
    . runOutputSem (embed . putChar)
    . evalState initialTape
    . runBrainfuckEffect
    . interpretBF
    $ ast

runBrainfuckProgramWithInput :: String -> String -> IO String
runBrainfuckProgramWithInput program input = do
  ast <-
    runFinal
      . throwErrorToIOFinal
      . parseBrainfuck
      $ program
  res <-
    runFinal
      . embedToFinal
      . throwErrorToIOFinal
      . (if False then traceToStdout else traceToNowhere)
      . runInputList input
      . runOutputMonoid (pure @[])
      . evalState initialTape
      . runBrainfuckEffect
      . interpretBF
      $ ast
  pure $ res ^. _1

helloWorld :: String
helloWorld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

-- >>> runBrainfuckProgramWithInput helloWorld ""
-- "Hello World!\n"
