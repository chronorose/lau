module Parser (Exp (..), parse) where

-- TODO: normal error handling with either.

import Lexer hiding (lex)

data Exp
  = Num Int
  | Func Function [Exp]
  deriving (Show)

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

parseImpl [] acc = reverse acc
parseImpl tokens acc = parseImpl rest (exp : acc)
  where
    (exp, rest) = parseExpr tokens

parse tokens = parseImpl tokens []
