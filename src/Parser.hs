module Parser where

import           Control.Monad.State
import           Data.List           (intercalate)
import           Lexer

data Expr   = Const Int
            | Var String
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Neg Expr
            | Assign String Expr
            | FunCall String [Expr]
            | FunDec String [String] Expr
            deriving (Eq)

instance Show Expr where
    show (Const a) = show a
    show (Var s) = s
    show (Add e e') = "(" ++ show e ++ " + " ++ show e' ++ ")"
    show (Sub e e') = "(" ++ show e ++ " - " ++ show e' ++ ")"
    show (Mul e e') = "(" ++ show e ++ " * " ++ show e' ++ ")"
    show (Div e e') = "(" ++ show e ++ " / " ++ show e' ++ ")"
    show (Neg e) = "-(" ++ show e ++ ")"
    show (Assign v e) = v ++ " = " ++ show e
    show (FunCall nm args) = nm ++ "(" ++ (intercalate ", " . map show $ args) ++ ")"
    show (FunDec nm args body) = nm ++ "(" ++ intercalate ", " args ++ ") {" ++ show body ++ "}"

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

runParser :: Parser a -> [Token] -> (a, [Token])
runParser = runState

fullParser :: Parser Expr
fullParser = topLevel

sepBy :: Show a => Parser a -> Token -> Parser [a]
sepBy inner sep = do
    el <- inner
    sep' <- peek
    if sep' == sep then do
        eat
        next <- sepBy inner sep
        return $ el:next
    else
        return [el]

parseIdent :: Parser String
parseIdent = do
    v <- eat
    case v of
        Ident nm -> return nm
        _ -> fail "expected identifier"

topLevel :: Parser Expr
topLevel = do
    dec <- peek
    case dec of
        Percent -> do
            eat
            funnm <- eat
            del <- eat
            case del of
                OPar ->
                    case funnm of
                        Ident nm -> do
                            args <- sepBy parseIdent Coma
                            eat
                            body <- expr
                            return $ FunDec nm args body
                        _ -> fail "function names must be plain strings"
                _ -> fail "%functionName(args) body"
        _ -> expr

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

term :: Parser Expr
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

factor :: Parser Expr
factor = do
    tok <- eat
    case tok of
        Op '+' -> factor
        Op '-' -> do
            nbr <- factor
            return $ Neg nbr
        TokNbr a -> return $ Const a
        Ident a -> do
            fun <- peek
            case fun of
                OPar -> do
                    eat
                    args <- funArgs
                    cpar <- eat
                    case cpar of
                        CPar -> return $ FunCall a args
                        _ -> fail "function call must end with ')'"
                _ -> return $ Var a
        OPar -> do
            e <- expr
            closing <- eat
            case closing of
                CPar -> return e
                _ -> fail "no matching closing parenthesis"
        _ -> fail "unexpected token"

funArgs :: Parser [Expr]
funArgs = do
    t <- peek
    if t == CPar then
        return []
    else do
        e <- expr
        coma <- peek
        if coma == Coma then do
            eat
            next <- funArgs
            return $ e:next
        else 
            return [e | coma == CPar ]
