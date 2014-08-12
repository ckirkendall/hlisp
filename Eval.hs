{-# LANGUAGE FlexibleContexts #-}
module Eval where

import Types
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map
import Control.Monad.Error


eval :: (MonadError LispError m) => Expression -> Env ->  m (Expression, Env)
eval (Identifier x) env = isValidLookup (lookup x env) x env
eval (Number x) env = return (Number x, env)
eval (Fn f) env = return (Fn f, env)
eval (SError s) env = throwError s
eval (SList quoted (h:t)) env = if quoted
                                   then return (SList quoted (h:t), env)
                                   else callFn env h t
                                  

safeEval :: Expression -> Env -> (Expression, Env)
safeEval exp env = case errorToExpression exp env of
  Right val -> val
  Left err -> (SError err, env)

errorToExpression :: Expression -> Env -> Either String (Expression, Env)
errorToExpression exp env = catchError (do (ex, en) <- eval exp env
                                           Right (ex, en))
                            (\err -> Left err)


isValidLookup :: (MonadError LispError m) => Maybe Expression -> String -> Env ->  m (Expression, Env)
isValidLookup Nothing x env = throwError ("var lookup failed for: " ++ x)
isValidLookup (Just val) _ env = return (val, env)


callFn :: (MonadError LispError m) => Env -> Expression -> [Expression] -> m (Expression, Env)
callFn env ident t = do
  i <- isIdentifier ident
  (v, e) <- eval i env
  f <- isFn v
  args <- evalFnArgs t env
  return (f args env)


isFn :: (MonadError LispError m) => Expression -> m ([Expression] -> Env -> (Expression, Env))
isFn (Fn f) = return f
isFn _ = throwError "invalid function call"


isIdentifier :: (MonadError LispError m) => Expression -> m Expression
isIdentifier (Identifier i) = return (Identifier i)
isIdentifier _ = throwError "expected an identifer"


evalFnArgs ::  (MonadError LispError m) => [Expression] -> Env -> m [Expression]
evalFnArgs [] env = return []
evalFnArgs (h:t) env = do
  (arg, nenv) <- eval h env
  res <- evalFnArgs t nenv
  return (arg : res)

