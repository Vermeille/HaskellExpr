module Main where

import           Eval
import           Parser
import           Lexer
import qualified Data.Map as M

evalLine :: Env -> IO ()
evalLine env = do
    input <- getLine
    let ast = fst . runParser fullParser . fst . tokenize $ input
    let (res, env', ioOut) = runEval (eval ast) () env
    putStrLn $ show ast ++ " = " ++ show res
    mapM_ execIO ioOut
    evalLine env'

evalSource :: String -> String -> (Int, Env, [IOAction])
evalSource input env =
    let ast = fst . runParser fullParser . fst . tokenize $ input
    in runEval (eval ast) () $ (read env :: SymTable, [])

execIO :: IOAction -> IO ()
execIO (Print str) = putStrLn str

main :: IO ()
main = do
    evalLine (M.empty, ["42", "84"])
