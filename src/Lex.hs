module Lex where

import qualified Data.Text as T
import qualified Data.Char as Char
import qualified Text.Regex as Regex
import qualified Token as Tok

charRegex :: Regex.Regex
charRegex = Regex.mkRegex "[a-zA-Z]+"

intRegex :: Regex.Regex
intRegex = Regex.mkRegex "[0-9]+"

lex :: T.Text -> [Tok.Tokens]
lex s = lexRest . T.strip $ s

lexRest :: T.Text -> [Tok.Tokens]
lexRest text
  | T.null text = []
  | T.head text == '{' = Tok.OpenBrace : (lexRest $ T.tail text)
  | T.head text == '}' = Tok.CloseBrace : (lexRest $ T.tail text)
  | T.head text == '(' = Tok.OpenParenthesis : (lexRest $ T.tail text)
  | T.head text == ')' = Tok.CloseParenthesis : (lexRest $ T.tail text)
  | T.head text == ';' = Tok.Semicolon : (lexRest $ T.tail text)
  | Char.isSpace $ T.head text = lexRest $ T.tail text
  | otherwise = lexRestAlphaNum text

lexRestAlphaNum :: T.Text -> [Tok.Tokens]
lexRestAlphaNum x = token : (lexRest $ T.pack rest)
  where intMatch = Regex.matchRegexAll intRegex (T.unpack x)
        (token, rest) = case intMatch of
            Just (a, b, c, _) ->
                if a == ""
                then (Tok.IntLiteral (read b), c)
                else (token', rest')
            Nothing -> (token', rest')
        charMatch = Regex.matchRegexAll charRegex (T.unpack x)
        (token', rest') = case charMatch of
            Just (a, b, c, _) ->
                if a == ""
                then (Tok.Identifier b, c)
                else error "No Match"
            Nothing -> error "No match"
