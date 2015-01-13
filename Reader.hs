module Reader where

import Types
import Text.ParserCombinators.Parsec
import Data.Maybe 

program :: Parser [Expression]
program =
    do first <- expression
       next <- remainingExpressions
       return (first : next)
       
remainingExpressions :: Parser [Expression]
remainingExpressions =
    (oneOf " ,\n" >> program)
    <|> return []

identifier :: Parser Expression
identifier = fmap Identifier $ many1 $ oneOf "&abcdefghijklmnopqrstuvwxyz_-+"

number :: Parser Expression
number = fmap (Number . read) $ many1 $ oneOf "1234567890"

sexp :: Parser Expression
sexp = do
    char '('
    exp <- program
    char ')'
    return $ SList exp

quote :: Parser Expression
quote = do
  char '\''
  exp <- expression
  return $ Quote exp

unquote :: Parser Expression
unquote = do
  char '~'
  exp <- expression
  return $ Unquote exp
  
--number = fmap (\x -> Number (read x :: Integer)) $ many $ oneOf "0123456789" 

--(+ 1 2 3)  (defn + [x & y 
 
expression :: Parser Expression
expression = quote
    <|> unquote
    <|> sexp 
    <|> number 
    <|> identifier

parseLisp :: String -> Either ParseError [Expression]
parseLisp = parse program "(unknown)"
