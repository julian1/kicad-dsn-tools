{-# LANGUAGE OverloadedStrings #-}

-- combine the 'exprParse' functions into one module

module ExprParser
    (
      exprParser
    , lexeme
    , varParser
    ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, skipSpace, char, double, string, anyChar, takeWhile1, letter)
import Data.Functor (($>))
import Data.Text (unpack)
import Lib (Expr(..))

exprParser :: Parser Expr
exprParser = boolParser <|> numParser <|> cmpParser <|> listParser <|> stringParser <|> varParser

-- | parse bool expression

boolParser :: Parser Expr
boolParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser

falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

notParser :: Parser Expr
notParser = do
    lexeme $ char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ char ')'
    return (Not expr)

andParser :: Parser Expr
andParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (And expr1 expr2)

orParser :: Parser Expr
orParser = do
    lexeme $ char '('
    lexeme $ string "or"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Or expr1 expr2)

-- | parse floating-point expression

numParser :: Parser Expr
numParser = doubleParser <|> addParser <|> minusParser <|> multParser <|> divParser

doubleParser :: Parser Expr
doubleParser = do
    skipSpace
    x <- double
    return (NumLit x)

addParser :: Parser Expr
addParser = do
    lexeme $ char '('
    lexeme $ char '+'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Add expr1 expr2)

minusParser :: Parser Expr
minusParser = do
    lexeme $ char '('
    lexeme $ char '-'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Minus expr1 expr2)

multParser :: Parser Expr
multParser = do
    lexeme $ char '('
    lexeme $ char '*'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Mult expr1 expr2)

divParser :: Parser Expr
divParser = do
    lexeme $ char '('
    lexeme $ char '/'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Div expr1 expr2)

-- | parse floating-point comparison

cmpParser :: Parser Expr
cmpParser = eqParser <|> lessParser <|> lessEqParser <|> greaterParser <|> greaterEqParser

eqParser :: Parser Expr
eqParser = do
    lexeme $ char '('
    lexeme $ char '='
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Eq expr1 expr2)

lessParser :: Parser Expr
lessParser = do
    lexeme $ char '('
    lexeme $ char '<'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Less expr1 expr2)

lessEqParser :: Parser Expr
lessEqParser = do
    lexeme $ char '('
    lexeme $ string "<="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (LessEq expr1 expr2)

greaterParser :: Parser Expr
greaterParser = do
    lexeme $ char '('
    lexeme $ char '>'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Greater expr1 expr2)

greaterEqParser :: Parser Expr
greaterEqParser = do
    lexeme $ char '('
    lexeme $ string ">="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (GreaterEq expr1 expr2)

-- | parse list

listParser :: Parser Expr
listParser = nilParser <|> consParser <|> carParser <|> cdrParser

nilParser :: Parser Expr
nilParser = lexeme $ string "nil" $> Nil

consParser :: Parser Expr
consParser = do
    lexeme $ char '('
    lexeme $ string "cons"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Cons expr1 expr2)

carParser :: Parser Expr
carParser = do
    lexeme $ char '('
    lexeme $ string "car"
    expr <- exprParser
    lexeme $ char ')'
    return (Car expr)

cdrParser :: Parser Expr
cdrParser = do
    lexeme $ char '('
    lexeme $ string "cdr"
    expr <- exprParser
    lexeme $ char ')'
    return (Cdr expr)

-- | parse string and char

stringParser :: Parser Expr
stringParser = charLiteralParser <|> strLiteralParser

charLiteralParser :: Parser Expr
charLiteralParser = do
    lexeme $ char '\''
    c <- anyChar
    char '\''
    return (CharLit c)

strParser :: Parser Expr
strParser = do
    c <- anyChar
    if c /= '\"'
        then do
            cs <- strParser
            return (Cons (CharLit c) cs)
        else return Nil

strLiteralParser :: Parser Expr
strLiteralParser = do
    lexeme $ char '\"'
    xs <- strParser
    return xs

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

-- | parse variabel

varParser :: Parser Expr
varParser = do
    skipSpace
    var_head <- letter
    var <- takeWhile1 (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c =='_' || c >= '0' && c <= '9')
    return (VarRef $ var_head : (unpack var))
