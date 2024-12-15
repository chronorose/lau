module Main where

import Parser
import System.Environment (getArgs)

parseArgs [] = error "Not enough arguments given."
parseArgs (file : _) = file

main :: IO ()
main = do
  args <- getArgs
  let fileName = parseArgs args
  file <- readFile fileName
  let (lexed, parsed, checked) = Parser.parseDebug file
  print "------------"
  print "Lexed: "
  print lexed
  print "------------"
  print "Parsed: "
  print parsed
  print "------------"
  print "Checked: "
  print checked
