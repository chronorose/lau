module Main where

import Parser (parseToAST)
import System.Environment (getArgs)

parseArgs [] = error "Not enough arguments given."
parseArgs (file : _) = file

main :: IO ()
main = do
  args <- getArgs
  let fileName = parseArgs args
  file <- readFile fileName
  print $ parseToAST file
