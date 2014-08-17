module Functions where

import Types
import Control.Monad.Error
import Data.Maybe
import Control.Exception
import Data.Map
import Eval

stdFn :: ([Expression] -> IO (Expression)) -> ([Expression] -> Env -> IO (Expression, Env))
stdFn fn = (\ exps env -> do
               nexps <- evalFnArgs exps env
               res <- fn nexps
               return (res, env))

------------------------------------------
-- Speical Forms
------------------------------------------

lispAdd :: [Expression] -> IO (Expression)
lispAdd [] = return (Number 0)
lispAdd (Number h : t) = do
  tail <- lispAdd t
  return (Number (h + fromMaybe 0 (getInteger tail)))
lispAdd a = throwIO (CodeError ("expected a number: " ++ (show a)))


lispLet :: [Expression] -> Env -> IO (Expression, Env)
lispLet ((SList exps):body) env = do
  nenv <- assignVarList exps env
  (res, _) <- evalBody body nenv
  return (res, env)
lispLet _ _ = throwIO (CodeError "let expects a list as the first argument")

--
lispFn :: [Expression] -> Env -> IO (Expression, Env)
lispFn ((SList args):body) env = return (Fn (\ params callEnv -> do 
                                                fnEnv <- mergeFnArgs args params callEnv env
                                                (res, fenv) <- evalBody body fnEnv
                                                return (res,callEnv)), env)
lispFn (h:t) _ = throwIO (CodeError "fn expects a list as first argument")


lispDef :: [Expression] -> Env -> IO (Expression, Env)
lispDef ((Identifier x):exp:[]) env = return (Unit, (insert x (LazyVar x exp) env))
lispDef a _ = throwIO (CodeError ("invalid def form: def " ++ (show a)))


lispIf :: [Expression] -> Env -> IO (Expression, Env)
lispIf (t:a:b:[]) env  = do
  res <- eval t env
  case res of
    (Boolean False, _) -> (eval b env)
    (Boolean True, _) -> (eval a env)
    (SList [],_) -> (eval b env)
    (Unit,_) -> (eval b env)
    (_,_) -> (eval a env)
lispIf _ _ = throwIO (CodeError "invalid if syntax")

lispCons :: [Expression] -> Env -> IO (Expression, Env) 
lispCons (a:e:[]) env = do
  (res, _) <- eval a env
  return ((LazySeq res e env), env)
lispCons a env = throwIO (CodeError ("cons expects an expression and a list:" ++ (show a)))


lispFirst :: [Expression] -> IO Expression
lispFirst ((SList exps):[]) = return (head exps)
lispFirst ((LazySeq a _ _):[]) = return a
lispFirst a = throwIO (CodeError ("first expects a list as its argument:" ++ (show a)))


lispRest :: [Expression] -> IO Expression
lispRest ((SList exps):[]) = return (SList (tail exps))
lispRest ((LazySeq _ exp env):[]) = do
  (res, _) <- eval exp env
  return res
lispRest a = throwIO (CodeError ("rest expects a list as its argument:" ++ (show a)))


lispPrint :: [Expression] -> IO Expression
listPrint (h:[]) = do
  res <- (realize h)
  putStr (show res)
  return Unit
lispPrint (h:t) = do
  res <- (realize h)
  putStr ((show res) ++ " ")
  lispPrint t
lispPrint [] = return Unit


lispMacro :: [Expression] -> Env -> IO (Expression, Env)
lispMacro ((SList args):body) env = return (Fn (\ params callEnv -> do 
                                                fnEnv <- mergeMacroArgs args params env
                                                (exp, fenv) <- evalBody body fnEnv
                                                (res, _) <- eval exp callEnv
                                                return (res,callEnv)), env)
lispMacro (h:t) _ = throwIO (CodeError "macro expects a list as first argument")


lispEval :: [Expression] -> Env -> IO (Expression, Env)
lispEval (h:[]) env = eval h env
lispEval _ _ = throwIO (CodeError "eval takes a single sexpression")

-----------------------------------------
-- HELPER FUNCTITONS
-----------------------------------------

mergeMacroArgs :: [Expression] -> [Expression] -> Env -> IO Env
mergeMacroArgs [] [] fnEnv = return fnEnv 
mergeMacroArgs (h:t) [] _ = throwIO (CodeError ("missing parameters: "++(show h)))
mergeMacroArgs [] (h:t) _ = throwIO (CodeError ("too many parameters passed: "++(show h)))
mergeMacroArgs ((Identifier x):xs) (y:ys) fnEnv = mergeMacroArgs xs ys (insert x y fnEnv)



mergeFnArgs :: [Expression] -> [Expression] -> Env -> Env -> IO Env
mergeFnArgs [] [] callEnv fnEnv = return fnEnv 
mergeFnArgs (h:t) [] _ _ = throwIO (CodeError ("missing parameters: "++(show h)))
mergeFnArgs [] (h:t) _ _ = throwIO (CodeError ("too many parameters passed: "++(show h)))
--mergeFnArgs ((Identifier "&"):h[]) callEnv fnEnv =
mergeFnArgs ((Identifier x):xs) (y:ys) callEnv fnEnv = do
  (nexp, _) <- eval y callEnv
  mergeFnArgs xs ys callEnv (insert x nexp fnEnv)



getInteger :: Expression -> Maybe Integer
getInteger (Number x) = Just x
getInteger y = Nothing


assignVarList :: [Expression] -> Env -> IO Env
assignVarList [] env = return env
assignVarList ((Identifier i):exp:t) env = assignVarList t (insert i (LazyVar i exp) env)
assignVarList a env = throwIO (CodeError ("expeced identifier here" ++ (show a)))


