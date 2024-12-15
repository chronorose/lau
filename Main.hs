module Main where

import Lexer
import System.Environment (getArgs)

parseArgs [] = error "Not enough arguments given."
parseArgs (file : _) = file

main :: IO ()
main = do
  args <- getArgs
  let fileName = parseArgs args
  file <- readFile fileName
  let lexed = Lexer.lex file
  print lexed
