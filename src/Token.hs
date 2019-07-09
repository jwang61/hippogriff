module Token where

data Tokens = OpenBrace
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
