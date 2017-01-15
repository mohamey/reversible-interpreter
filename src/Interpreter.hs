{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-} 

module Interpreter where

import Prelude hiding (lookup, print)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List.Split

import qualified System.IO as System

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

data Val = I Int | B Bool
           deriving (Eq, Show, Read)

data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or Expr Expr | Not Expr 
     | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
     | Var String
   deriving (Eq, Show, Read)

type Name = String
type Env = Map.Map Name Val

lookup k t = case Map.lookup k t of
               Just x -> return x
               Nothing -> fail ("Unknown variable "++k)

{-- Monadic style expression evaluator,
 -- with error handling and Reader monad instance to carry dictionary
 --}

type Eval a = ReaderT Env (ExceptT String Identity) a
-- runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

-- Evaluate integer expressions
evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> fail "type error in arithmetic expression"

-- Evaluate Boolean Expressions
evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> fail "type error in boolean expression"

-- Operations over integers which produce booleans
evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

-- Evaluate an expression
eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do evalb (const not) e0 (Const (B True)) 
                       where not2 a _ = not a -- hack, hack

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env


{-------------------------------------------------------------------}
{- The statement language                                          -}


data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Pass
      deriving (Eq, Show, Read)

-- evalStatement :: Statement -> Eval Val
-- evalStatement (Assign var expr) = do
  -- let res = eval expr

type Run a = StateT Env (ExceptT String IO) a
runRun r = runExceptT ( runStateT r Map.empty)

-- Update a value in the env
set :: Name -> Val -> Run ()
set var val = state $ (\table -> ( (), Map.insert var val table))

-- Take a statement and execute it
execStatement :: Statement -> Run ()
execStatement (Assign var expr) = do
  env <- get
  Right val <- return $ runEval env $ eval expr -- Eval expression then set it in env
  set var val

execStatement (If expr stmt1 stmt2) = do
  env <- get
  Right val <- return $ runEval env $ eval expr -- Evaluate condition
  case val of
    (B True) -> execStatement stmt1 -- If it's true, execute first statement
    (B False) -> execStatement stmt2 -- If it's false, execute second statement
    _ -> fail "Expression did not resolve to boolean value"

execStatement (While expr stmt) = do
  env <- get
  Right val <- return $ runEval env $ eval expr
  case val of
    (B True) -> execStatement stmt >> -- If condition is true, execute first statement
      execStatement (While expr stmt) -- Execute while loop again
    (B False) -> return () -- If condition is false, return nothing
    _ -> fail "Expression did not resolve to boolean value"

-- Evaluate expression then print it
execStatement (Print expr) = do
  env <- get
  Right val <- return $ runEval env $ eval expr
  liftIO $ putStrLn $ show val
  return ()

-- Sequence to statements together
execStatement (Seq stmt1 stmt2) = do
  execStatement stmt1 >> execStatement stmt2

-- Try catch block
execStatement (Try stmt1 stmt2) = do
  catchError (execStatement stmt1) (\_ -> execStatement stmt2)

execStatement Pass = do return ()

-- Look up variables from env
inspectVar :: Name -> Run ()
inspectVar var = do
  env <- get
  val <- lookup var env
  case val of
    (I _) -> liftIO $ putStrLn $ show val
    (B _) -> liftIO $ putStrLn $ show val

-- Parse file's string to statements
stringToStatements :: [String] -> [Statement]
stringToStatements = map read

-- Print a list of statements
printStatements :: [Statement] -> IO ()
printStatements [] = return ()
printStatements (x:xs) = do
  putStrLn $ show x
  printStatements xs

startProgram :: String -> Run ()
startProgram fn = do
  fileString <- liftIO $ readFile fn
  -- putStrLn $ fileString
  let (x:xs) = stringToStatements $ lines fileString -- Get statements
  execStatement x
  handleStatements xs
  -- putStrLn "Finished"
  -- printStatements stmts


handleStatements :: [Statement] -> Run ()
handleStatements [] = return ()
handleStatements (x:xs) = do
  -- liftIO $ putStrLn $ show x
  liftIO $ putStrLn "Enter a command"
  command <- liftIO $ getLine
  let cmdParts = splitOn " " command
  case head cmdParts of
    "step" -> execStatement x >> handleStatements xs
    "inspect" -> inspectVar (cmdParts !! 1) >> handleStatements (x:xs)
    _ -> return ()

