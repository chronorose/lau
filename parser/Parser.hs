module Parser where

-- TODO: infix into exprs. module. cleanup.

import Data.Char (isAlphaNum, isDigit, isLetter)
import Data.List (find, intercalate)
import qualified Data.Maybe
import Debug.Trace (trace)
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
  deriving (Eq)

instance Show Expr where
  show (Num i) = "(i64 " ++ show i ++ ")"
  show (Name str) = "(i64 " ++ str ++ ")"
  show (InfixFunc name e1 e2) = show e1 ++ name ++ show e2
  show (FuncCall name es) = name ++ "(" ++ intercalate ", " (map show es) ++ ")"
  show (Func t name params stmts) = t ++ " " ++ name ++ "(" ++ showParams params ++ ")\n" ++ statements
    where
      showParams [] = ""
      showParams [(name, t)] = t ++ " " ++ name
      showParams ((name, t) : params) = t ++ " " ++ name ++ ", " ++ showParams params
      statements = concatMap show stmts

data Stmt
  = Assign String Expr
  | Declare String Expr
  | For Expr [Stmt]
  | If Expr [Stmt]
  deriving (Eq)

instance Show Stmt where
  show (Assign var exp) = var ++ " = " ++ show exp ++ "\n"
  show (Declare var exp) = var ++ " := " ++ show exp ++ "\n"

funcName :: Parsec String st String
funcName = do
  spaces
  first <- noneOf ['0' .. '9']
  rest <- many $ satisfy (\c -> isAlphaNum c || c == '_')
  spaces
  let m = first : rest
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
  fname <- funcName
  char '('
  arglist <- try expr `sepBy` (spaces >> char ',' >> spaces)
  char ')'
  return (FuncCall fname arglist)

var = do
  spaces
  w <- word
  spaces
  return (Name w)

uexpr e1 = do
  (fname, e2) <- do
    s <- funcName
    trace (show s) return ()
    e <- expr
    return (s, e)
  return (InfixFunc fname e1 e2)

expr = do
  spaces
  exp <- try numExpr <|> try fnCall <|> var
  trace (show exp) return ()
  inf <- optionMaybe $ try $ uexpr exp
  case inf of
    Just e -> return e
    Nothing -> return exp

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

parseToAST = parse func "something went wrong while parsing function"
