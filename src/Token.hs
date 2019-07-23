module Token where

import qualified Data.Text as T

data Token = OpenBrace
            | CloseBrace
            | OpenParenthesis
            | CloseParenthesis
            | Semicolon
            | IntKeyword
            | CharKeyword
            | ReturnKeyword
            | Identifier T.Text
            | IntLiteral Int
            | CharLiteral Char
            deriving (Show, Eq)
