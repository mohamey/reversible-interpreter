module Main where

import Lib
import Interpreter

import System.Environment

runProgram :: String -> Run ()
runProgram name = startProgram name

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  runProgram fileName
  putStrLn "Finished"

