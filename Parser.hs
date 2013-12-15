{-# LANGUAGE OverloadedStrings #-}

module Parser (definitions) where

import Control.Applicative
import Text.Trifecta  hiding (token)
import Text.Parser.Token.Style
import qualified Data.HashSet as HashSet

import AST

tok :: (Monad m, TokenParsing m) => String -> m String
tok =  token . string

token :: (Monad m, TokenParsing m) => m a -> m a
token t = t <* many (char ' ')

expression :: (Monad m, TokenParsing m) => m Expr
expression = app 

app :: (Monad m, TokenParsing m) => m Expr
app = foldl1 App <$> some (try prim)

prim :: (Monad m, TokenParsing m) => m Expr
prim = (Val . IntValue) <$> integer
    <|> (Val . BoolValue) <$> bool
    <|> ifExpr
    <|> Var <$> identifier
    <|> lambda
    <|> parens expression

ifExpr :: (Monad m, TokenParsing m) => m Expr
ifExpr = reserved "if" *> (If <$> expression
                         <* reserved "then"
                         <*> expression
                         <*  reserved "else"
                         <*> expression)

bool :: (Monad m, TokenParsing m) => m Bool
bool = True <$ tok "Yes" <|> False <$ tok "No"

lambda :: (Monad m, TokenParsing m) => m Expr
lambda = tok "\\" *> (
          Lam <$> identifier
              <* tok "->"
              <*> expression)

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

identifier :: (Monad m, TokenParsing m) => m String
identifier = ident style

style :: (Monad m, TokenParsing m) => IdentifierStyle m
style = emptyIdents { _styleReserved = HashSet.fromList $ words "if then else Yes No" }

definition :: (Monad m, TokenParsing m) => m Definition
definition = Definition <$> identifier 
                        <* token (string "=")
                        <*> expression
                        <* semi

definitions :: (Monad m, TokenParsing m) => m [Definition]
definitions = many definition <* eof
