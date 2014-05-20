module HaskellExpr.Eval where

import Data.List (intercalate)
import HaskellExpr.Parser
import Control.Monad.RWS.Strict
import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))

data IOAction   = Print String
                deriving (Show)

type SymTable = M.Map String Expr

type Env = (SymTable, [String])
type Evaluator = RWS () [IOAction] Env

readVar :: String -> Evaluator Expr
readVar var = do
    env <- fromEnv
    case M.lookup var env of
        Just val -> return val
        Nothing -> error $ "cannot find variable " ++ var

fromEnv :: Evaluator SymTable
fromEnv = get >>= return . fst

toEnv :: (SymTable -> SymTable) -> Evaluator ()
toEnv f = modify (\(env, io) -> (f env, io))

toIO :: ([String] -> [String]) -> Evaluator ()
toIO f = modify (\(env, io) -> (env, f io))

fromIO :: Evaluator [String]
fromIO = get >>= return . snd

writeVar :: String -> Expr -> Evaluator ()
writeVar var val = do
    toEnv (M.insert var val)
    return ()

runEval :: Evaluator a -> () -> Env -> (a, Env, [IOAction])
runEval = runRWS

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
eval (FunCall nm args)
    | nm == "print" = do
        a' <- mapM eval args
        tell [Print $ intercalate ", " $ map show a']
        return 0
    | nm == "read" = do
        input <- fromIO
        toIO tail
        return $ read (head input)
    | otherwise = do
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
