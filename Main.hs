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
  res <- startEval (parseLisp code) initEnv
  putStrLn res


code = "(def x 3) " ++
       "(def l '(1 2 3)) " ++
       "(def map (fn (f c) " ++
                    "(if c " ++
                      "(cons (f (first c)) (map f (rest c))) " ++
                      "c))) " ++
       "(def reduce (fn (f i c) " ++
                     "(if c " ++
                       "(reduce f (f i (first c)) (rest c)) " ++
                       "i))) " ++
       "(print (map (fn (x) (+ 1 x)) l)) " ++
       "(print (reduce (fn (x y) (+ x y)) 0 l)) " ++
       "(let (a (fn (d e) (+ d e))) (+ x (a 1 2)))" 


initEnv :: Env
initEnv = fromList [("+", Fn (stdFn lispAdd)),
                    ("let", Fn lispLet),
                    ("fn", Fn lispFn),
                    ("def", Fn lispDef),
                    ("if", Fn lispIf),
                    ("cons", Fn lispCons),
                    ("first", Fn (stdFn lispFirst)),
                    ("rest", Fn (stdFn lispRest)),
                    ("print", Fn (stdFn lispPrint))]


startEval :: Either ParseError [Expression] -> Env -> IO (String) 
startEval (Left err) _ = return (show err)
startEval (Right exps) env = do (exp, env) <- evalBody exps env
                                res <- (realize exp)
                                return (show res)

