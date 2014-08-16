module Main where

import Text.ParserCombinators.Parsec
import Types
import Reader
import Eval 
import Data.Map
import Functions
import Control.Exception

main :: IO ()
main = do
  res <- (startEval (parseLisp "(+ 1 (+ 1 2))") (fromList [("+", Fn (stdFn lispAdd))]))
  putStrLn res
  
startEval :: Either ParseError [Expression] -> Env -> IO (String) 
startEval (Left err) _ = return (show err)
startEval (Right (h:t)) env = do (exp, env) <- eval h env
                                 return (show exp)
                                 
                              

