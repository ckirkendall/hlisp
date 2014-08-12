module Functions where

import Types
import Control.Monad.Error
import Data.Maybe

stdFn :: ([Expression] -> Expression) -> ([Expression] -> Env -> (Expression, Env))
stdFn fn = (\ exps env -> (fn exps, env))

lispAdd :: [Expression] -> Expression
lispAdd [] = Number 0
lispAdd (Number h : t) = Number (h + fromMaybe 0 (getInteger (lispAdd t)))

getInteger :: Expression -> Maybe Integer
getInteger (Number x) = Just x
getInteger y = Nothing


                   
                  
