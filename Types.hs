{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Map
import Control.Exception
import Data.Typeable.Internal

data LispError = CodeError String
               | ApplicationError Expression String deriving (Show,Typeable)

instance Exception LispError

data Expression = SList Bool [Expression] --first arg is for quoted or not 
                | Identifier String
                | Number Integer
                | Fn ([Expression] -> Env -> IO (Expression,Env))

instance Show Expression where
  show (Identifier a) = a
  show (Number a) = show a
  show (Fn b) = "*FN*"
  show (SList quoted a) = if quoted
                             then "'(" ++ (show a) ++ ")"
                             else "(" ++ (show a) ++ ")"     


type Env = Map String Expression
