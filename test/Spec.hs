module Main where

import qualified Data.Text as T
import           Test.HUnit

import qualified Lex as L
import qualified Token as Tok

testLexInteger :: Test
testLexInteger =
  TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
  where expected = [Tok.IntKeyword 123]
        inputStr = (T.pack "123")

testLexCharKeyword :: Test
testLexCharKeyword =
  TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
  where expected = [Tok.CharKeyword "return"]
        inputStr = (T.pack "return")

tests :: Test
tests = TestList [ testLexInteger
                 , testLexCharKeyword
                 ]

main :: IO Counts
main = runTestTT tests
