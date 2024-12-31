module Main where

import Parser
import qualified System.Exit as Exit
import Test.HUnit
import Text.Parsec

test1 =
  TestCase
    ( assertEqual
        "test basic expression"
        (Right $ FuncCall "+" [Num 3, Num 4])
        (parse expr "" "+(3, 4);")
    )

tests = TestList [TestLabel "test1" test1]

main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
