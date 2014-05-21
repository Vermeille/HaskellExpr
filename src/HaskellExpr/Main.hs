module Main where

import HaskellExpr.Eval
import HaskellExpr.Parser
import HaskellExpr.Lexer
import qualified Data.Map as M

evalLine :: Env -> IO ()
evalLine env = do
    input <- getLine
    let ast = fst . runParser fullParser . map fst . tokenize $ input
    let (res, env', ioOut) = runEval (eval ast) () env
    putStrLn $ show ast ++ " = " ++ show res
    mapM_ execIO ioOut
    evalLine env'

execIO :: IOAction -> IO ()
execIO (Print str) = putStrLn str

main :: IO ()
main = do
    evalLine (M.empty, ["42", "84"])
