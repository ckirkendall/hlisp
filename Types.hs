module Types where

import Data.Map

type LispError = String

data Expression = SList Bool [Expression] --first arg is for quoted or not 
                | Identifier String
                | Number Integer
                | Fn ([Expression] -> Env -> (Expression,Env))
                | SError String

instance Show Expression where
  show (Identifier a) = a
  show (Number a) = show a
  show (Fn b) = "*FN*"
  show (SError s) = s
  show (SList quoted a) = if quoted
                             then "'(" ++ (show a) ++ ")"
                             else "(" ++ (show a) ++ ")"     


type Env = Map String Expression
