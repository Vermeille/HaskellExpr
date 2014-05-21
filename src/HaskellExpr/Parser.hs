module HaskellExpr.Parser where

import           Control.Monad.State
import           Data.List           (intercalate)
import HaskellExpr.Lexer
import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)

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

instance Read Expr where
        readsPrec _ str = [(fst parsed, last . map snd . take (length (snd parsed)+1) . reverse $ toks)]
                where
                    toks = tokenize str
                    parsed = runParser fullParser $ map fst toks

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
    show (FunDec nm args body) = "%" ++ nm ++ "(" ++ intercalate ", " args ++ ") " ++ show body

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

sepBy :: Parser a -> Token -> Parser [a]
sepBy inner sep = do
    el <- inner
    sep' <- peek
    if sep' == sep then
        (el:) <$> (eat *> sepBy inner sep)
    else
        return [el]

token :: Token -> Parser Token
token t = do
    t' <- eat
    case (t, t') of
        (Ident _, Ident _) -> return t'
        (TokNbr _, TokNbr _) -> return t'
        (a, b) -> if a == b then return t' else
            fail $ "expecting " ++ show t ++ " but found " ++ show t'

betweenOp :: Token -> Token -> Parser a -> Parser [a]
betweenOp opening closing inner = do
    _ <- token opening
    nxt <- peek
    if nxt == closing then
        eat *> return []
    else do
        body <- inner
        _ <- token closing
        return [body]

between :: Token -> Token -> Parser a -> Parser a
between opening closing inner = do
    _ <- token opening
    body <- inner
    _ <- token closing
    return body

parseIdent :: Parser String
parseIdent = do
    Ident nm <- token $ Ident "var"
    return nm

topLevel :: Parser Expr
topLevel = do
    dec <- peek
    case dec of
        Percent -> FunDec <$> (eat *> parseIdent) <*> args <*> body
             where
                args = (betweenOp OPar CPar $ sepBy parseIdent Coma) >>= return . concat
                body = expr
        _ -> expr

expr :: Parser Expr
expr = do
    t <- term
    op <- peek
    case op of
        Op '+' -> Add <$> pure t <*> (eat *> expr)
        Op '-' -> Sub <$> pure t <*> (eat *> expr)
        Op '=' ->
            case t of
                Var nm -> Assign <$> pure nm <*> (eat *> expr)
                _ -> fail "can only assign to a lvalue"
        _ -> return t

term :: Parser Expr
term = do
    lhs <- factor
    op <- peek
    case op of
        Op '*' -> Mul <$> (eat *> pure lhs) <*> term
        Op '/' -> Div <$> (eat *> pure lhs) <*> term
        _ -> return lhs

factor :: Parser Expr
factor = do
    tok <- eat
    case tok of
        Op '+' -> factor
        Op '-' -> Neg <$> factor
        TokNbr a -> return $ Const a
        Ident a -> do
            fun <- peek
            case fun of
                OPar -> FunCall a <$> (concat <$> betweenOp OPar CPar funArgs)
                _ -> return $ Var a
        OPar -> expr <* token CPar
        _ -> fail "unexpected token"

funArgs :: Parser [Expr]
funArgs = sepBy expr Coma
