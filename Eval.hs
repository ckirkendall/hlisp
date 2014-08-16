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
eval (SList quoted (h:t)) env = if quoted
                                   then return (SList quoted (h:t), env)
                                   else callFn env h t
                                  


isValidLookup :: Maybe Expression -> String -> Env -> IO (Expression, Env)
isValidLookup Nothing x env = throwIO (CodeError ("var lookup failed for: " ++ x))
isValidLookup (Just val) _ env = return (val, env)


callFn :: Env -> Expression -> [Expression] -> IO (Expression, Env)
callFn env ident t = do
  i <- isIdentifier ident
  (v, e) <- eval i env
  f <- isFn v
  args <- evalFnArgs t env
  (f args env)


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

