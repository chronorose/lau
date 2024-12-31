module Lexer (Lexer.lex, Function (..), Token (..)) where

-- TODO: refactor it with Either so that implementation tells you exact place in which
-- incorrect input took place and doesn't 'just' crash. for now it's fine.
-- TODO: parser combinators.

import Data.Char

data Function
  = Plus
  | Minus
  | Multiply
  | Read
  deriving (Show, Eq)

data Token
  = LeftParen
  | RightParen
  | Number Int
  | Keyword Function
  deriving (Show, Eq)

symbols = ['(', ')', '+', '-', '*']

keywords = ["read"]

isSymbol :: Char -> Bool
isSymbol s = s `elem` symbols

isParen '(' = True
isParen ')' = True
isParen _ = False

symbolMatch :: String -> (Maybe Token, String)
symbolMatch (s : str) = (Just token, str)
  where
    token =
      case s of
        ')' -> RightParen
        '(' -> LeftParen
        '+' -> Keyword Plus
        '-' -> Keyword Minus
        '*' -> Keyword Multiply
        _ -> error ("unimplemented symbol" ++ [s])

isKeyword word = word `elem` keywords

matchKeyword word =
  case word of
    "read" -> Keyword Read
    _ -> error ("unimplemented keyword" ++ word)

wordMatch :: String -> (Maybe Token, String)
wordMatch str
  | isKeyword word = (Just $ matchKeyword word, rest)
  | otherwise = error "incorrect keyword."
  where
    (word, rest) = break (\s -> isSpace s || isParen s) str

numberMatch :: String -> (Maybe Token, String)
numberMatch str = (Just $ Number $ read number, rest)
  where
    (number, rest) = span isNumber str

match :: String -> (Maybe Token, String)
match (s : str)
  | isSpace s = (Nothing, str)
  | isNumber s = numberMatch (s : str)
  | Lexer.isSymbol s = symbolMatch (s : str)
  | otherwise = wordMatch (s : str)

lexImpl :: String -> [Token] -> [Token]
lexImpl [] tokens = tokens
lexImpl (s : str) tokens =
  case token of
    Just t -> lexImpl rest (t : tokens)
    Nothing -> lexImpl rest tokens
  where
    (token, rest) = match (s : str)

lex str = reverse $ lexImpl str []
