{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Funny.Brainfuck where

import Control.Lens (Field1 (_1), (%~), (&), (+~), (-~), (.~), (<&>), (^.))
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
import Polysemy (Member, Members, Sem, embed, embedToFinal, interpret, makeSem, reinterpret, runFinal, run)
import Polysemy.Error (Error, throw, runError)
import Polysemy.Input (Input (Input), input, runInputList, runInputSem)
import Polysemy.Output (Output, output, runOutputMonoid, runOutputSem)
import Polysemy.State (State, evalState, gets, modify)
import Polysemy.Trace (Trace (Trace), trace, traceToStdout)
import Text.Megaparsec (Parsec, between, choice, many, satisfy, sepBy)
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
parser :: Parser [BFCommand]
parser = ignoreComment *> (commandParser `sepBy` ignoreComment) <* ignoreComment

-- Parses individual Brainfuck commands, ignoring comments
commandParser :: Parser BFCommand
commandParser =
  choice
    [ BFCMoveRight <$ char '>'
    , BFCMoveLeft <$ char '<'
    , BFCIncrement <$ char '+'
    , BFCDecrement <$ char '-'
    , BFCOutput <$ char '.'
    , BFCInput <$ char ','
    , BFCLoop <$> between (char '[') (char ']') (parser)
    ]

-- Ignore any character that is not a valid Brainfuck command
ignoreComment :: Parser ()
ignoreComment = void $ many (satisfy isCommentChar)
  where
    isCommentChar :: Char -> Bool
    isCommentChar c = c `notElem` ['>', '<', '+', '-', '.', '[', ']', ',']

-- Function to parse a Brainfuck program from a string
parseBrainfuck :: (Member (Error String) r) => String -> Sem r [BFCommand]
parseBrainfuck input = parse parser "kek" $ T.pack input

parseBrainfuck' :: String -> Either String [BFCommand]
parseBrainfuck' input =
  parseBrainfuck input
    & runError
    & run

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

makeSem ''BrainfuckEffect

-- Interpreter for AST
interpretBF ::
  (Members '[BrainfuckEffect, Trace] r) =>
  [BFCommand] ->
  Sem r ()
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

allowFiniteInput ::
  forall i r a.
  (Member (Error String) r) =>
  Sem (Input i ': r) a ->
  Sem (Input (Maybe i) ': r) a
allowFiniteInput = reinterpret \case
  Input -> do
    mi <- input @(Maybe i)
    case mi of
      Nothing -> throw "End of input"
      Just i -> pure i

-- Main BrainfuckEffect handler
runBrainfuckEffect ::
  (Members '[State Tape, Input Char, Output Char, Trace] r) =>
  Sem (BrainfuckEffect ': r) a ->
  Sem r a
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
    trace $ "Output char: " ++ show (chr val)
  InputChar -> do
    c <- input
    modify $ #current .~ ord c
    trace $ "Input char: " ++ show c
  GetCurrentValue -> do
    v <- gets (^. #current)
    trace $ "Got current value: " ++ show v
    pure v

-- Ignore traces
traceToNowhere ::
  Sem (Trace ': r) a ->
  Sem r a
traceToNowhere = interpret \case
  Trace _msg -> return ()

runBrainfuckProgram :: String -> IO ()
runBrainfuckProgram program = do
  ast <-
    program
      & parseBrainfuck
      & throwErrorToIOFinal
      & runFinal
  ast
    & interpretBF
    & runBrainfuckEffect
    & (if False then traceToStdout else traceToNowhere)
    & evalState initialTape
    & runOutputSem (embed . putChar)
    & runInputSem (embed getChar)
    & embedToFinal
    & runFinal

runBrainfuckProgramWithInput :: String -> String -> IO Text
runBrainfuckProgramWithInput program input = do
  ast <-
    program
      & parseBrainfuck
      & throwErrorToIOFinal
      & runFinal
  ast
    & interpretBF
    & runBrainfuckEffect
    & (if False then traceToStdout else traceToNowhere)
    & evalState initialTape
    & runOutputMonoid T.singleton <&> (^. _1)
    -- NOTE: puts `Input (Maybe i)` on top
    & allowFiniteInput @Char
    & runInputList input
    & throwErrorToIOFinal
    & embedToFinal
    & runFinal

runBrainfuckProgramWithInputPure :: String -> String -> Either String Text
runBrainfuckProgramWithInputPure program input = do
  ast <-
    program
      & parseBrainfuck
      & runError
      & run
  ast
    & interpretBF
    & runBrainfuckEffect
    & traceToNowhere
    & evalState initialTape
    & runOutputMonoid T.singleton <&> (^. _1)
    & allowFiniteInput @Char
    & runInputList input
    & runError
    & run

-- Examples

helloWorld :: String
helloWorld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

-- >>> runBrainfuckProgramWithInputPure helloWorld ""
-- Right "Hello World!\n"

alphabet :: String
alphabet = ",>++++++++++[<.+>-]"

-- >>> runBrainfuckProgramWithInputPure alphabet "a"
-- Right "abcdefghij"

-- >>> runBrainfuckProgramWithInputPure alphabet ""
-- Left "End of input"

-- NOTE: source <http://www.linusakesson.net/programming/brainfuck>
-- FIXME: doesn't parse
life :: String
life = " \
\                             Linus Akesson presents:                               \n\
\                    The Game Of Life implemented in Brainfuck                      \n\
\                                                                                   \n\
\        +>>++++[<++++>-]<[<++++++>-]+[<[>>>>+<<<<-]>>>>[<<<<+>>>>>>+<<-]<+         \n\
\    +++[>++++++++<-]>.[-]<+++[>+++<-]>+[>>.+<<-]>>[-]<<<++[<+++++>-]<.<<[>>>>+     \n\
\  <<<<-]>>>>[<<<<+>>>>>>+<<-]<<[>>>>.+<<<++++++++++[<[>>+<<-]>>[<<+>>>>>++++++++   \n\
\  +++<<<-]<[>+<-]>[<+>>>>+<<<-]>>>[>>>>>>>>>>>>+>+<<     <<<<<<<<<<<-]>>>>>>>>>>   \n\
\ >>[-[>>>>+<<<<-]>[>>>>+<<<<-]>>>]>      >>[<<<+>>  >-    ]<<<[>>+>+<<<-]>[->[<<<  \n\
\ <+>>>>-]<[<<<  <+>      >>>-]<<<< ]<     ++++++  ++       +[>+++++<-]>>[<<+>>-]<  \n\
\ <[>---<-]>.[- ]         <<<<<<<<< <      <<<<<< <         -]++++++++++.[-]<-]>>>  \n\
\ >[-]<[-]+++++           +++[>++++        ++++<     -     ]>--.[-]<,----------[<+  \n\
\ >-]>>>>>>+<<<<< <     <[>+>>>>>+>[      -]<<<      <<   <<-]>++++++++++>>>>>[[-]  \n\
\ <<,<<<<<<<->>>> >    >>[<<<<+>>>>-]<<<<[>>>>+      >+<<<<<-]>>>>>----------[<<<<  \n\
\ <<<<+<[>>>>+<<<      <-]>>>>[<<<<+>>>>>>+<<-      ]>[>-<-]>++++++++++[>+++++++++  \n\
\ ++<-]<<<<<<[>>>      >+<<<<-]>>>>[<<<<+>>>>>      >+<<-]>>>>[<<->>-]<<++++++++++  \n\
\ [>+<-]>[>>>>>>>      >>>>>+>+<<<<      <<<<<      <<<<-]>>> >>     >>>>>>>[-[>>>  \n\
\ >+<<<<-]>[>>>>       +<<<<-]>> >       ]>> >           [<< <        +>>>-]+<<<[>  \n\
\ >>-<<<-]>[->[<      <<<+>>>>-]         <[ <            < <           <+>>>>-]<<<  \n\
\ <]<<<<<<<<<<<, [    -]]>]>[-+++        ++               +    +++     ++[>+++++++  \n\
\ ++++>+++++++++ +    +<<-]>[-[>>>      +<<<-      ]>>>[ <    <<+      >>>>>>>+>+<  \n\
\ <<<<-]>>>>[-[> >    >>+<<<<-]>[>      >>>+< <    <<-]> >    >]>      >>[<<<+>>>-  \n\
\ ]<<<[>>+>+<<< -     ]>[->[<<<<+>      >>>-] <    [<<< <    +>>       >>-]<<<<]<<  \n\
\ <<<<<<[>>>+<< <     -]>>>[<<<+>>      >>>>> +    >+<< <             <<-]<<[>>+<<  \n\
\ -]>>[<<+>>>>>      >+>+<<<<<-]>>      >>[-[ >    >>>+ <            <<<-]>[>>>>+<  \n\
\ <<<-]>[>>>>+<      <<<-]>>]>>>[ -    ]<[>+< -    ]<[ -           [<<<<+>>>>-]<<<  \n\
\ <]<<<<<<<<]<<      <<<<<<<<++++ +    +++++  [   >+++ +    ++++++[<[>>+<<-]>>[<<+  \n\
\ >>>>>++++++++ +    ++<<<     -] <    [>+<- ]    >[<+ >    >>>+<<<-]>>>[<<<+>>>-]  \n\
\ <<<[>>>+>>>>  >    +<<<<     <<      <<-]> >    >>>>       >>>[>>+<<-]>>[<<+<+>>  \n\
\ >-]<<<------ -    -----[     >>      >+<<< -    ]>>>       [<<<+> > >>>>>+>+<<<<  \n\
\ <-]>>>>[-[>> >    >+<<<<    -] >     [>>>> +    <<<<-       ]>>> ]  >>>[<<<+>>>-  \n\
\ ]<<<[>>+>+<< <    -]>>>     >>           > >    [<<<+               >>>-]<<<[>>>  \n\
\ +<<<<<+>>-                  ]>           >     >>>>>[<             <<+>>>-]<<<[>  \n\
\ >>+<<<<<<<                  <<+         >      >>>>>-]<          <<<<<<[->[<<<<+  \n\
\ >>>>-]<[<<<<+>>>>-]<<<<]>[<<<<<<    <+>>>      >>>>-]<<<<     <<<<<+++++++++++[>  \n\
\ >>+<<<-]>>>[<<<+>>>>>>>+>+<<<<<-]>>>>[-[>     >>>+<<<<-]>[>>>>+<<<<-]>>>]>>>[<<<  \n\
\ +>>>-]<<<[>>+>+<<<-]>>>>>>>[<<<+>>>-]<<<[     >>>+<<<<<+>>-]>>>>>>>[<<<+>>>-]<<<  \n\
\ [>>>+<<<<<<<<<+>>>>>>-]<<<<<<<[->[< <  <     <+>>>>-]<[<<<<+>>>>-]<<<<]>[<<<<<<<  \n\
\ +>>>>>>>-]<<<<<<<<<+++++++++++[>>> >        >>>+>+<<<<<<<<-]>>>>>>>[-[>>>>+<<<<-  \n\
\ ]>[>>>>+<<<<-]>>>]>>>[<<<+>>>-]<<< [       >>+>+<<<-]>>>>>>>[<<<+>>>-]<<<[>>>+<<  \n\
\ <<<+>>-]>>>>>>>[<<<+>>>-]<<<[>>>+<        <<<<<<<<+>>>>>>-]<<<<<<<[->[<<<<+>>>>-  \n\
\  ]<[<<<<+>>>>-]<<<<]>[<<<<<<<+>>>>>      >>-]<<<<<<<----[>>>>>>>+<<<<<<<+[>>>>>   \n\
\  >>-<<<<<<<[-]]<<<<<<<[>>>>>>>>>>>>+>+<<<<<<<<<<<<<-][   lft@df.lth.se   ]>>>>>   \n\
\    >>>>>>>[-[>>>>+<<<<-]>[>>>>+<<<<-]>[>>>>+<<<<-]>>]>>>[-]<[>+<-]<[-[<<<<+>>     \n\
\        >>-]<<<<]<<<<<<[-]]<<<<<<<[-]<<<<-]<-]>>>>>>>>>>>[-]<<]<<<<<<<<<<]         \n\
\                                                                                   \n\
\         Type for instance 'fg' to toggle the cell at row f and column g           \n\
\                    Hit enter to calculate the next generation                     \n\
\                                  Type q to quit                                   \n\
\ "
