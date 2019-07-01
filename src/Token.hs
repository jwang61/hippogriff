module Token where

data Tokens = OpenBrace
            | CloseBrace
            | OpenParenthesis
            | CloseParenthesis
            | Semicolon
            | IntKeyword Int
            | CharKeyword String
            | ReturnKeyword
            | Identifier
            | IntLiteral Int
            deriving (Show, Eq)
