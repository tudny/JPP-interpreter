module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import Src.Jabba.Abs   (Program)
import Src.Jabba.Lex   ( Token, mkPosToken )
import Src.Jabba.Par   ( pProgram, myLexer )
import Src.Jabba.Print ( Print, printTree )
import Src.Jabba.Skel  ()
import Src.TypeChecker ( typeCheck )
import Src.Evaluator   ( evaluate )

type Err        = Either String
type ParseFun a = [Token] -> Err a

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: ParseFun Program -> String -> IO ()
run p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      putStrLn "TypeChecking..."
      case typeCheck tree of
        Left err -> do
          putStrLn "\nTypeCheck          Failed...\n"
          putStrLn err
          exitFailure
        Right tree -> do
          putStrLn "\nTypeCheck Successful!"
          putStrLn "\nInterpreting..."
          case evaluate tree of
            Left err -> do
              putStrLn "\nInterpret          Failed...\n"
              putStrLn err
              exitFailure
            Right io -> do
              putStrLn "\nInterpret Successful!"
              io
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin."
    , "  (filename)         Parse content of a file."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run pProgram
    [f]        -> runFile pProgram f
    t          -> usage

