module Main where

import Lib
import Interpreter

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  runRun $ startProgram fileName
  putStrLn "Finished"

