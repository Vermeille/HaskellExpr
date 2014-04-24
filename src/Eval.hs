module Eval where

import Parser
import Control.Monad.State
import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))

type Env = M.Map String Expr
type Evaluator a = State Env a

readVar :: String -> Evaluator Expr
readVar var = do
    env <- get
    case M.lookup var env of
        Just val -> return val
        Nothing -> error $ "cannot find variable " ++ var

writeVar :: String -> Expr -> Evaluator ()
writeVar var val = do
    modify (M.insert var val)
    return ()

runEval :: State Env a -> Env -> (a, Env)
runEval = runState

eval :: Expr -> Evaluator Int
eval (Const a) = return a
eval (Var v) = do
    (Const val) <- readVar v
    return val

eval (Add e e') = (+) <$> eval e <*> eval e'
eval (Sub e e') = (-) <$> eval e <*> eval e'
eval (Mul e e') = (*) <$> eval e <*> eval e'
eval (Div e e') = div <$> eval e <*> eval e'
eval (Neg e) = negate <$> eval e
eval (Assign var e) = do
    e' <- eval e
    writeVar var (Const e')
    return e'
eval (FunCall nm args) = do
    env <- get
    (FunDec _ decArgs body) <- readVar nm
    forM_  (zip args decArgs) $ \(v, n) -> do
        v' <- eval v
        writeVar n (Const v')
    ret <- eval body
    put env
    return ret
eval fun@(FunDec nm _ _) = do
    writeVar nm fun
    return 0
