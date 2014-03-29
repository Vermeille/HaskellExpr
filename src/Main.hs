import           Eval
import           Parser
import           Lexer
import qualified Data.Map as M

evalLine :: Env -> IO ()
evalLine env = do
    input <- getLine
    let ast = fst . runParser expr . tokenize $ input
    let res = runEval (eval ast) env
    putStrLn $ show ast ++ " = " ++ (show . fst $ res)
    evalLine . snd $ res

main :: IO ()
main = evalLine M.empty
