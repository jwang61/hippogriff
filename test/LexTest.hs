module LexTest (lexTests) where

import qualified Data.Text as T
import           Test.HUnit

import qualified Lex as L
import qualified Token as Tok

testLexInteger :: Test
testLexInteger =
  TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
  where expected = [Tok.IntLiteral 123]
        inputStr = (T.pack "123")

testLexIdentifierAlpha :: Test
testLexIdentifierAlpha =
  TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
  where expected = [Tok.Identifier "foo"]
        inputStr = (T.pack "foo")

testLexIntKeyword :: Test
testLexIntKeyword =
  TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
  where expected = [Tok.IntKeyword]
        inputStr = (T.pack "int")

testLexIdentifierAlphaNum :: Test
testLexIdentifierAlphaNum =
  TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
  where expected = [Tok.Identifier "myvar3"]
        inputStr = (T.pack "myvar3")

testLexIdentifierAlphaNumUnderscore :: Test
testLexIdentifierAlphaNumUnderscore =
  TestCase $ assertEqual ("Lexing: " ++ show inputStr) expected (L.lex inputStr)
  where expected = [Tok.Identifier "_myvar3"]
        inputStr = (T.pack "_myvar3")

lexTests :: Test
lexTests = TestList [ testLexInteger
                 , testLexIdentifierAlpha
                 , testLexIdentifierAlphaNum
                 , testLexIdentifierAlphaNumUnderscore
                 , testLexIntKeyword
                 ]
