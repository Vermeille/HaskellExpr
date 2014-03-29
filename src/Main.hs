import           Control.Monad (liftM, liftM2)
import           Eval
import           Parser
import           Lexer
import qualified Data.Map as M

evalLine env = do
    input <- getLine
    let ast = fst . runParser expr . tokenize $ input
    let res = runEval (eval ast) env
    putStrLn $ show ast ++ " = " ++ (show . fst $ res)
    evalLine . snd $ res

main = evalLine M.empty
