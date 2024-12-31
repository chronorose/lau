module Parser where

import Data.Char (isAlphaNum, isDigit, isLetter)
import Data.List (find)
import qualified Data.Maybe
import Text.Parsec
import qualified Text.Printf as Parsec
import qualified Text.Printf as String

keywordList = [("for", For), ("if", If)]

type Type = String

type FuncParams = (String, Type)

data Expr
  = Num Int
  | Name String
  | Func Type String [FuncParams] [Stmt]
  | InfixFunc String Expr Expr
  | FuncCall String [Expr]
  deriving (Show)

data Stmt
  = Assign String Expr
  | Declare String Expr
  | For Expr [Stmt]
  | If Expr [Stmt]
  deriving (Show)

funcName :: Parsec String st String
funcName = do
  spaces
  first <- noneOf ['0' .. '9']
  rest <- many letter
  spaces
  return (first : rest)

word :: Parsec String st String
word = do
  spaces
  first <- satisfy (\c -> isLetter c || c == '_')
  rest <- many $ satisfy (\c -> isAlphaNum c || c == '_')
  spaces
  return (first : rest)

func = do
  string "fnc"
  fname <- funcName
  arguments <- args
  spaces
  ttype <- optionMaybe (string "->" *> word)
  spaces
  char '{'
  statements <- stmts
  char '}'
  let ttype_ret = Data.Maybe.fromMaybe "void" ttype
  return (Func ttype_ret fname arguments statements)

stmts = do
  many (try declare <|> assign)

declare = do
  var <- word
  string ":="
  expression <- expr
  spaces
  char ';'
  spaces
  return (Declare var expression)

assign = do
  var <- word
  string "="
  expression <- expr
  spaces
  char ';'
  spaces
  return (Assign var expression)

args = do
  char '('
  arglist <- arg `sepBy` char ','
  char ')'
  return arglist

arg = do
  name <- word
  char ':'
  ttype <- word
  return (name, ttype)

numExpr = do
  n <- many1 digit
  return $ Num $ read n

fnCall = do
  fname <- word
  char '('
  arglist <- expr `sepBy` (spaces >> char ',' >> spaces)
  char ')'
  return (FuncCall fname arglist)

expr = do
  spaces
  numExpr <|> fnCall

infixFunc = do
  arg1 <- expr
  f <- funcName
  InfixFunc f arg1 <$> expr

wordOrKeyword =
  let kwSearch lword = find (\(str, _) -> str == lword) keywordList
   in let checkForKeyword lword = case kwSearch lword of
            Just (str, _) -> Name str -- have to parse for exact keyword here
            Nothing -> Name lword
       in checkForKeyword <$> word

p = parse func "something went wrong while parsing function"
