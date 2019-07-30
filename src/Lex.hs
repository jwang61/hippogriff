{-# LANGUAGE OverloadedStrings #-}

module Lex (Lex.lex) where

import           Data.Foldable (asum)
import qualified Data.Char as Char
import qualified Data.Text as T
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
  | firstChar == '{' = Tok.OpenBrace : rest
  | firstChar == '}' = Tok.CloseBrace : rest
  | firstChar == '(' = Tok.OpenParenthesis : rest
  | firstChar == ')' = Tok.CloseParenthesis : rest
  | firstChar == ';' = Tok.Semicolon : rest
  | firstChar == '-' = Tok.Negation : rest
  | firstChar == '~' = Tok.BitwiseComplement : rest
  | firstChar == '!' = Tok.LogicalNegation : rest
  | Char.isSpace $ firstChar = rest
  | otherwise = lexRestAlphaNum text
  where firstChar = T.head text
        rest = lexRest $ T.tail text

tryRegexMatch :: Regex.Regex -> (String -> Tok.Token) -> T.Text -> Maybe (Tok.Token, T.Text)
tryRegexMatch regex strToTok x =
  let regMatch = Regex.matchRegexAll regex (T.unpack x)
      matchesToTok (a, b, c, _) = if a == "" then Just (strToTok b, T.pack c) else Nothing
  in regMatch >>= matchesToTok

lexRestAlphaNum :: T.Text -> [Tok.Token]
lexRestAlphaNum x = token : (lexRest rest)
  where regexes = [ tryRegexMatch intRegex (Tok.IntLiteral . read)
                  , tryRegexMatch idRegex (identifierOrKeyword . T.pack)
                  ]
        matchResult = asum $ map ($ x) regexes
        (token, rest) = case matchResult of
           Just (a, b) -> (a, b)
           Nothing -> error "No Matches"

identifierOrKeyword :: T.Text -> Tok.Token
identifierOrKeyword str
  | str == "int" = Tok.IntKeyword
  | str == "char" = Tok.CharKeyword
  | str == "return" = Tok.ReturnKeyword
  | otherwise = Tok.Identifier str
