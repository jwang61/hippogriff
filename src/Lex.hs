{-# LANGUAGE OverloadedStrings #-}

module Lex (Lex.lex) where

import qualified Data.Text as T
import qualified Data.Char as Char
import qualified Text.Regex as Regex
import qualified Token as Tok

lex :: T.Text -> [Tok.Token]
lex s = lexRest . T.strip $ s

idRegex :: Regex.Regex
idRegex = Regex.mkRegex "^[_a-zA-Z][_a-zA-Z0-9]*"

intRegex :: Regex.Regex
intRegex = Regex.mkRegex "[0-9]+"

lexRest :: T.Text -> [Tok.Token]
lexRest text
  | T.null text = []
  | T.head text == '{' = Tok.OpenBrace : (lexRest $ T.tail text)
  | T.head text == '}' = Tok.CloseBrace : (lexRest $ T.tail text)
  | T.head text == '(' = Tok.OpenParenthesis : (lexRest $ T.tail text)
  | T.head text == ')' = Tok.CloseParenthesis : (lexRest $ T.tail text)
  | T.head text == ';' = Tok.Semicolon : (lexRest $ T.tail text)
  | T.head text == '-' = Tok.Negation : (lexRest $ T.tail text)
  | T.head text == '~' = Tok.BitwiseComplement : (lexRest $ T.tail text)
  | T.head text == '!' = Tok.LogicalNegation : (lexRest $ T.tail text)
  | Char.isSpace $ T.head text = lexRest $ T.tail text
  | otherwise = lexRestAlphaNum text

lexRestAlphaNum :: T.Text -> [Tok.Token]
lexRestAlphaNum x = token : (lexRest $ T.pack rest)
  where intMatch = Regex.matchRegexAll intRegex (T.unpack x)
        (token, rest) = case intMatch of
            Just (a, b, c, _) ->
                if a == ""
                then (Tok.IntLiteral (read b), c)
                else (token', rest')
            Nothing -> (token', rest')
        idMatch = Regex.matchRegexAll idRegex (T.unpack x)
        (token', rest') = case idMatch of
            Just (a, b, c, _) ->
                if a == ""
                then (identifierOrKeyword (T.pack b), c)
                else error "No Match"
            Nothing -> error "No match"

identifierOrKeyword :: T.Text -> Tok.Token
identifierOrKeyword str
  | str == "int" = Tok.IntKeyword
  | str == "char" = Tok.CharKeyword
  | str == "return" = Tok.ReturnKeyword
  | otherwise = Tok.Identifier str
