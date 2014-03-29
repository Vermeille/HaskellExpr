module Parser where

import           Control.Monad.State
import           Lexer

data Expr   = Const Int
            | Var String
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Neg Expr
            | Assign String Expr

instance Show Expr where
    show (Const a) = show a
    show (Var s) = s
    show (Add e e') = "(" ++ show e ++ " + " ++ show e' ++ ")"
    show (Sub e e') = "(" ++ show e ++ " - " ++ show e' ++ ")"
    show (Mul e e') = "(" ++ show e ++ " * " ++ show e' ++ ")"
    show (Div e e') = "(" ++ show e ++ " / " ++ show e' ++ ")"
    show (Neg e) = "-(" ++ show e ++ ")"
    show (Assign v e) = v ++ " = " ++ show e

type Parser a = State [Token] a

peek :: Parser Token
peek = do
    toks <- get
    return $ head toks

eat :: Parser Token
eat = do
    toks <- get
    put $ tail toks
    return $ head toks

runParser = runState

expr :: Parser Expr
expr = do
    t <- term
    op <- peek
    case op of
        Op '+' -> do
            eat
            rhs <- expr
            return $ Add t rhs
        Op '-' -> do
            eat
            rhs <- expr
            return $ Sub t rhs
        Op '=' ->
            case t of
                Var nm -> do
                    eat
                    rhs <- expr
                    return $ Assign nm rhs
                _ -> fail "can only assign to a lvalue"
        _ -> return t

term = do
    lhs <- factor
    op <- peek
    case op of
        Op '*' -> do
            eat
            rhs <- term
            return $ Mul lhs rhs
        Op '/' -> do
            eat
            rhs <- term
            return $ Div lhs rhs
        _ -> return lhs

factor = do
    tok <- eat
    case tok of
        Op '+' -> factor
        Op '-' -> do
            nbr <- factor
            return $ Neg nbr
        TokNbr a -> return $ Const a
        Ident a -> return $ Var a
        OPar -> do
            e <- expr
            closing <- eat
            case closing of
                CPar -> return e
                _ -> fail "no matching closing parenthesis"
