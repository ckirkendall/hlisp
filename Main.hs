module Main where

import Text.ParserCombinators.Parsec
import Types
import Reader
import Eval 
import Data.Map
import Control.Monad.Error
import Functions

main :: IO ()
main = putStrLn $ showResults $ startEval (parseLisp "(+ 1 (+ 1 2))") (fromList [("+", Fn (stdFn lispAdd))])


startEval :: Either ParseError [Expression] -> Env -> Either LispError String
startEval (Left err) _ = Left (show err)
startEval (Right (h:t)) env = catchError (do (r, e) <- eval h env
                                             return (show r))
                              (\err -> return (show err))
 

showResults :: Either LispError String -> String
showResults (Left a) = "ERROR: " ++ a
showResults (Right a) = a
