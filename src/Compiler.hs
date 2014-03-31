module Compiler where
{-

import Control.Monad.State
import Data.Sequence as S
import           Parser

data AsmReg = EAX
            | EBX
            | ECX
            | EDX
            deriving (Eq)

instance Show AsmReg where
    show EAX = "%eax"
    show EBX = "%ebx"
    show ECX = "%ecx"
    show EDX = "%edx"

data AsmMem = Stack Int
            | Ram Int

instance Show AsmMem where
    show (Stack offset) = show offset ++ "(%ebp)"
    show (Ram addr) = "(" ++ show addr ++ ")"

data AsmEnv = AsmEnv {
    usedRegs :: [(String, AsmReg)],
    freeRegs :: [AsmReg],
    addrOf   :: S.Seq String
}

baseAsmEnv :: AsmEnv
baseAsmEnv = AsmEnv { usedRegs = [], freeRegs = [EAX, EBX, ECX, EDX], addrOf = S.empty}

type AsmEmit a = State AsmEnv a

allocStack :: String -> AsmEmit AsmMem
allocStack var = do
    stack <- get
    let idx' = S.elemIndexL var $ addrOf stack
    case idx' of
        Nothing -> do
            put $ stack { addrOf = (var <|) $ addrOf stack }
            return $ Stack 0
        Just offset -> return $ Stack (offset * 4)

allocReg :: String -> AsmEmit AsmReg
allocReg var = do
    regs <- get
    case freeRegs regs of
        r:rs -> do
            put $ regs { usedRegs = (var, r):usedRegs regs, freeRegs = rs }
            return r
        [] -> do
            let new = snd . last . usedRegs $ regs
            put $ regs { usedRegs = (var, new): (init . usedRegs $ regs), freeRegs = [new] }
            return new

compile :: Expr -> AsmEmit String
compile (Const a) = return $ "mov $" ++ show a ++ ", " ++ "%eax\n"
-}