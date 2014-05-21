module HaskellExpr.Lexer where

import Data.Char(isAlpha, isDigit)

data Token  = TokNbr Int
            | Op Char
            | Ident String
            | OPar
            | CPar
            | EOF
            | Coma
            | Percent
            deriving (Eq, Show)

tokenize :: String -> [(Token, String)]
tokenize [] = [(EOF, "")]
tokenize (' ':cs) = tokenize cs
tokenize ('+':cs) = (Op '+', cs):tokenize cs
tokenize ('-':cs) = (Op '-', cs):tokenize cs
tokenize ('*':cs) = (Op '*', cs):tokenize cs
tokenize ('/':cs) = (Op '/', cs):tokenize cs
tokenize ('=':cs) = (Op '=', cs):tokenize cs
tokenize ('(':cs) = (OPar, cs):tokenize cs
tokenize (')':cs) = (CPar, cs):tokenize cs
tokenize (',':cs) = (Coma, cs):tokenize cs
tokenize ('%':cs) = (Percent, cs):tokenize cs
tokenize ('\n':cs) = [(EOF, cs)]
tokenize cs | isDigit . head $ cs =
                let (val, cs') = atoi cs
                in (TokNbr val, cs'):tokenize cs'
            | isAlpha . head $ cs =
                let ident = takeWhile isAlpha cs
                    cs' = drop (length ident) cs
                in (Ident ident, cs'):tokenize cs'
            | otherwise = [(EOF, cs)]

atoi :: String -> (Int, String)
atoi cs = let valStr = takeWhile isDigit cs
          in (read valStr, drop (length valStr) cs)
