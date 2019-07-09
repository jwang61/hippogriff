module Lex where

import qualified Data.Text as T
import qualified Data.Char as Char
import qualified Text.Regex as Regex
import qualified Token as Tok

idRegex :: Regex.Regex
idRegex = Regex.mkRegex "^[_a-zA-Z][_a-zA-Z0-9]*"

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
        idMatch = Regex.matchRegexAll idRegex (T.unpack x)
        (token', rest') = case idMatch of
            Just (a, b, c, _) ->
                if a == ""
                then (identifierOrKeyword b, c)
                else error "No Match"
            Nothing -> error "No match"

identifierOrKeyword :: String -> Tok.Tokens
identifierOrKeyword str
  | str == "return" = Tok.ReturnKeyword
  | str == "int" = Tok.IntKeyword
  | str == "char" = Tok.CharKeyword
  | otherwise = Tok.Identifier str
