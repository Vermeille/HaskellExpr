module Eval where

import Parser
import Control.Monad.State
import qualified Data.Map as M

type Env = M.Map String Int
type Evaluator a = State Env a

readVar :: String -> Evaluator Int
readVar var = do
    env <- get
    case M.lookup var env of
        Just val -> return val
        Nothing -> error $ "cannot find variable " ++ var

writeVar :: String -> Int -> Evaluator ()
writeVar var val = do
    modify (M.insert var val)
    return ()

runEval :: State Env a -> Env -> (a, Env)
runEval = runState

eval :: Expr -> Evaluator Int
eval (Const a) = return a
eval (Var v) = readVar v
eval (Add e e') = do
    lhs <- eval e
    rhs <- eval e'
    return $ lhs + rhs
eval (Sub e e') = do
    lhs <- eval e
    rhs <- eval e'
    return $ lhs - rhs
eval (Mul e e') = liftM2 (*) (eval e) (eval e')
eval (Div e e') = liftM2 div (eval e) (eval e')
eval (Neg e) = liftM negate (eval e)
eval (Assign var e) = do
    e' <- eval e
    writeVar var e'
    return e'
