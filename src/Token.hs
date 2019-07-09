module Token where

data Token = OpenBrace
            | CloseBrace
            | OpenParenthesis
            | CloseParenthesis
            | Semicolon
            | IntKeyword
            | CharKeyword
            | ReturnKeyword
            | Identifier String
            | IntLiteral Int
            | CharLiteral Char
            deriving (Show, Eq)
