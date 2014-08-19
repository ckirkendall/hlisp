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
eval (Quote exp) env = evalQuote exp env
eval (SList (h:t)) env = callFn env h t
eval (LazySeq head rest lenv) env = do
  res <- (realize (LazySeq head rest lenv))
  eval res env
eval (Thunk exp env) oenv = realizeThunks (Thunk exp env) oenv
    

evalBody :: [Expression] -> Env -> IO (Expression, Env)
evalBody (h:[]) env = return ((Thunk h env), env)
evalBody (h:t) env = do
  (res,nenv) <- eval h env
  evalBody t nenv

evalVarArgs :: [Expression] -> Env -> IO [Expression]
evalVarArgs [] env = return []
evalVarArgs (h:t) env = do
  (res, nenv) <- eval h env
  tres <- evalVarArgs t nenv
  return (res:tres)   

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
  (targ,tenv) <- realizeThunks arg nenv
  res <- evalFnArgs t tenv
  return (targ : res)


realizeThunks :: Expression -> Env -> IO (Expression, Env)
realizeThunks (Thunk exp env) _ = do
  (res, _) <- eval exp env
  case res of 
    (Thunk exp env) -> eval (Thunk exp env) env
    exp -> return (exp,env)
realizeThunks exp env = return (exp, env)

realize :: Expression -> IO Expression
realize (Thunk exp env) = do
 (res, _) <- realizeThunks (Thunk exp env) env
 realize res
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
  show (Unit) = "*unit*"
  show (Boolean b) = (show b)
  show (Quote a) = "'" ++ (show a)
  show (Unquote a) = "~" ++ (show a)
  show (LazyVar i e) = "lazy("++(show e)++")"
  show (LazySeq a exp env) = "*LazySeq*"
  show (Thunk e env) = "thunk("++(show e)++")"
  show (SList (h:t)) = "(" ++
                       show(h) ++
                       (Prelude.foldl (\ start exp -> (start ++ " " ++ show(exp))) "" t) ++ 
                       ")"
  show (SList []) = "()"
                                 

