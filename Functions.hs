module Functions where

import Types
import Control.Monad.Error
import Data.Maybe
import Control.Exception

stdFn :: ([Expression] -> IO (Expression)) -> ([Expression] -> Env -> IO (Expression, Env))
stdFn fn = (\ exps env -> do res <- fn exps
                             return (res, env))

lispAdd :: [Expression] -> IO (Expression)
lispAdd [] = return (Number 0)
lispAdd (Number h : t) = do tail <- lispAdd t
                            return (Number (h + fromMaybe 0 (getInteger tail)))
lispAdd a = throwIO (CodeError ("expected a number: " ++ (show a)))

getInteger :: Expression -> Maybe Integer
getInteger (Number x) = Just x
getInteger y = Nothing


                   
                  
