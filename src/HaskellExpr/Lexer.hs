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

tokenize :: String -> ([Token], String)
tokenize [] = ([EOF], "")
tokenize (' ':cs) = tokenize cs
tokenize ('+':cs) = Op '+' `lexThen` tokenize cs
tokenize ('-':cs) = Op '-' `lexThen` tokenize cs
tokenize ('*':cs) = Op '*' `lexThen` tokenize cs
tokenize ('/':cs) = Op '/' `lexThen` tokenize cs
tokenize ('=':cs) = Op '=' `lexThen` tokenize cs
tokenize ('(':cs) = OPar `lexThen` tokenize cs
tokenize (')':cs) = CPar `lexThen` tokenize cs
tokenize (',':cs) = Coma `lexThen` tokenize cs
tokenize ('%':cs) = Percent `lexThen` tokenize cs
tokenize ('\n':cs) = ([EOF], cs)
tokenize cs | isDigit . head $ cs =
                let (val, cs') = atoi cs
                in TokNbr val `lexThen` tokenize cs'
            | isAlpha . head $ cs =
                let ident = takeWhile isAlpha cs
                in Ident ident `lexThen` tokenize (drop (length ident) cs)
            | otherwise = ([], cs)

lexThen t (t', str') = (t:t', str')

atoi :: String -> (Int, String)
atoi cs = let valStr = takeWhile isDigit cs
          in (read valStr, drop (length valStr) cs)
