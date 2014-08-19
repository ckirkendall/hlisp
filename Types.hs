{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Map
import Control.Exception
import Data.Typeable.Internal

data LispError = CodeError String
               | ApplicationError Expression String deriving (Typeable)


data Expression = SList [Expression] --first arg is for quoted or not 
                | Quote Expression
                | Unquote Expression
                | Identifier String
                | Number Integer
                | Boolean Bool
                | Unit
                | LazyVar String Expression
                | LazySeq Expression Expression Env
                | Thunk Expression Env
                | Fn ([Expression] -> Env -> IO (Expression,Env))



type Env = Map String Expression
