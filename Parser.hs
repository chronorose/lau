module Parser (Exp (..), parse, parseDebug) where

-- TODO: normal error handling with either.
-- devise architecture of parser in which unary ops work well.
-- (doesn't mean they don't work there, but certainly not good enough)

import Lexer

data Exp
  = Num Int
  | Func Function [Exp]
  deriving (Show, Eq)

arity exp =
  case exp of
    Read -> 0
    Plus -> 2
    Multiply -> 2

checkForArity (Num _) = True
checkForArity (Func t expList) = (arity t == length expList) && checkExprsForArity expList

checkExprsForArity :: [Exp] -> Bool
checkExprsForArity = all checkForArity

checks = [checkExprsForArity]

allChecks exps = all (\check -> check exps) checks

check :: [Exp] -> Maybe [Exp]
check exps
  | allChecks exps = Just exps
  | otherwise = Nothing

expectRParen :: Exp -> [Token] -> (Exp, [Token])
expectRParen expr (t : tokens)
  | t == RightParen = (expr, tokens)
  | otherwise = error "incorrect input."

parseRest k (t : tokens) acc =
  case t of
    RightParen -> (Func k (reverse acc), tokens)
    Number i -> parseRest k tokens (Num i : acc)
    LeftParen ->
      let (exp, rest_tokens) = parseExpr (t : tokens)
       in parseRest k rest_tokens (exp : acc)
    _ -> error "incorrect input."

parseFirst :: [Token] -> (Exp, [Token])
parseFirst (t : tokens) =
  case t of
    Number n -> expectRParen (Num n) tokens
    Keyword k -> parseRest k tokens []
    _ -> error "incorrect input."

-- i do not check (almost) anything semantically on correctness at this point.
-- so everything that comes out of here can be as semantically unsound as possible.
-- also it is pretty dumb tbh. will probably need to refactor the shit out of it, but i cba to think rn.
parseExpr :: [Token] -> (Exp, [Token])
parseExpr (t : tokens) =
  case t of
    LeftParen -> parseFirst tokens
    _ -> error "Incorrect expression."

parseImpl acc [] = reverse acc
parseImpl acc tokens = parseImpl (exp : acc) rest
  where
    (exp, rest) = parseExpr tokens

parse :: String -> Maybe [Exp]
parse = check . parseImpl [] . Lexer.lex

parseDebug str = (lexed, parsed, checked)
  where
    lexed = Lexer.lex str
    parsed = parseImpl [] lexed
    checked = check parsed
