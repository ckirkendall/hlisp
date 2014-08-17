module Reader where

import Types
import Text.ParserCombinators.Parsec
import Data.Maybe 

program :: GenParser Char st [Expression]
program =
    do first <- expression
       next <- remainingExpressions
       return (first : next)
remainingExpressions :: GenParser Char st [Expression]
remainingExpressions =
    (oneOf " ,\n" >> program)
    <|> return []

identifier :: GenParser Char st Expression
identifier = fmap Identifier $ many1 $ oneOf "&abcdefghijklmnopqrstuvwxyz_-+"

number :: GenParser Char st Expression
number = fmap (Number . read) $ many1 $ oneOf "1234567890"

sexp :: GenParser Char st Expression
sexp = do
    char '('
    exp <- program
    char ')'
    return $ SList exp

quote :: GenParser Char st Expression
quote = do
  char '\''
  exp <- expression
  return $ Quote exp

unquote :: GenParser Char st Expression
unquote = do
  char '~'
  exp <- expression
  return $ Unquote exp
  
--number = fmap (\x -> Number (read x :: Integer)) $ many $ oneOf "0123456789" 

--(+ 1 2 3)  (defn + [x & y 
 
expression :: GenParser Char st Expression
expression = quote
    <|> unquote
    <|> sexp 
    <|> number 
    <|> identifier

parseLisp :: String -> Either ParseError [Expression]
parseLisp = parse program "(unknown)"
