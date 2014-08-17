{-# LANGUAGE FlexibleContexts #-}
module Eval where

import Types
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map
import Control.Exception


eval :: Expression -> Env ->  IO (Expression, Env)
eval (Identifier x) env = isValidLookup (lookup x env) x env
eval (Number x) env = return (Number x, env)
eval (Fn f) env = return (Fn f, env)
eval Unit env = return (Unit, env)
eval (Quote exp) env = return (exp, env)
eval (SList (h:t)) env = callFn env h t


evalBody :: [Expression] -> Env -> IO (Expression, Env)
evalBody (h:[]) env = eval h env
evalBody (h:t) env = do
  (res,nenv) <- eval h env
  evalBody t nenv


evalQuote :: Expression -> Env -> IO (Expression, Env)
evalQuote (SList (h:t)) env = do
  (res, env) <- evalUnquote h env
  ((SList tres), env) <- evalQuote (SList t) env
  return ((SList (res:tres)),env)
evalQuote exp env = return (exp, env)


evalUnquote :: Expression -> Env -> IO (Expression, Env)
evalUnquote (Unquote exp) env = eval exp env
evalUnquote exp env = evalQuote exp env


isValidLookup :: Maybe Expression -> String -> Env -> IO (Expression, Env)
isValidLookup Nothing x env = do putStrLn (show env)
                                 throwIO (CodeError ("var lookup failed for: " ++ x))
isValidLookup (Just (LazyVar a exp)) _ env = do
  (res, _) <- eval exp env
  return (res, (insert a res env))
isValidLookup (Just val) _ env = return (val, env)


callFn :: Env -> Expression -> [Expression] -> IO (Expression, Env)
callFn env ident t = do
  i <- isIdentifier ident
  (v, e) <- eval i env
  f <- isFn v
  (f t env)


isFn :: Expression -> IO ([Expression] -> Env -> IO (Expression, Env))
isFn (Fn f) = return f
isFn _ = throwIO (CodeError "invalid function call")


isIdentifier :: Expression -> IO (Expression)
isIdentifier (Identifier i) = return (Identifier i)
isIdentifier _ = throwIO (CodeError "expected an identifer")


evalFnArgs :: [Expression] -> Env -> IO ([Expression])
evalFnArgs [] env = return []
evalFnArgs (h:t) env = do
  (arg, nenv) <- eval h env
  res <- evalFnArgs t nenv
  return (arg : res)


realize :: Expression -> IO Expression
realize (LazySeq a exp env) = do 
   (res, _) <- (eval exp env)
   t <- (realize res)
   case t of
     (SList b) -> return (SList (a:b))
     _ -> throwIO (CodeError "invalid lazy sequence")
realize (SList a) = return (SList a)
realize exp = return exp

instance Exception LispError

instance Show LispError where
  show (CodeError s) = ("Error: " ++ s)
  show (ApplicationError exp str) = ("Error: " ++ (show exp) ++ "->" ++ str)

instance Show Expression where
  show (Identifier a) = a
  show (Number a) = show a
  show (Fn b) = "*FN*"
  show (Unit) = ""
  show (Quote a) = "'" ++ (show a)
  show (LazyVar i e) = show e
  show (LazySeq a exp env) = "*LazySeq*"
  show (SList (h:t)) = "(" ++
                       show(h) ++
                       (Prelude.foldl (\ start exp -> (start ++ " " ++ show(exp))) "" t) ++ 
                       ")"
  show (SList []) = "()"
                                 

