module Main where

import qualified Data.Text as T
import           Test.HUnit

import qualified Lex as L
import qualified Token as Tok

testLexInteger :: Test
testLexInteger =
  TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
  where expected = [Tok.IntLiteral 123]
        inputStr = (T.pack "123")

testLexIdentifier :: Test
testLexIdentifier =
  TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
  where expected = [Tok.Identifier "foo"]
        inputStr = (T.pack "foo")

-- testLexIntKeyword :: Test
-- testLexIntKeyword =
--   TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
--   where expected = [Tok.IntKeyword]
--         inputStr = (T.pack "int")

-- testLexCharAndIntKeyword :: Test
-- testLexCharAndIntKeyword =
--   TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
--   where expected = [Tok.CharKeyword "my_var3"]
--         inputStr = (T.pack "my_var3")

tests :: Test
tests = TestList [ testLexInteger
                 , testLexIdentifier
                --  , testLexIntKeyword
                --  , testLexCharAndIntKeyword
                 ]

main :: IO Counts
main = runTestTT tests
