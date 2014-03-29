module Lexer where

import Data.Char(isAlpha, isDigit)

data Token  = TokNbr Int
            | Op Char
            | Ident String
            | OPar
            | CPar
            | EOF

tokenize :: String -> [Token]
tokenize [] = [EOF]
tokenize (' ':cs) = tokenize cs
tokenize ('+':cs) = Op '+':tokenize cs
tokenize ('-':cs) = Op '-':tokenize cs
tokenize ('*':cs) = Op '*':tokenize cs
tokenize ('/':cs) = Op '/':tokenize cs
tokenize ('=':cs) = Op '=':tokenize cs
tokenize ('(':cs) = OPar:tokenize cs
tokenize (')':cs) = CPar:tokenize cs
tokenize ('\n':cs) = [EOF]
tokenize cs | isDigit . head $ cs =
                let (val, cs') = atoi cs
                in TokNbr val:tokenize cs'
            | isAlpha . head $ cs =
                let ident = takeWhile isAlpha cs
                in Ident ident:tokenize (drop (length ident) cs)
            | otherwise = error "lexing error"

atoi :: String -> (Int, String)
atoi cs = let valStr = takeWhile isDigit cs
          in (read valStr, drop (length valStr) cs)
