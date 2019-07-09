module AST where

data ReturnType = IntType
                | CharType
                deriving (Show, Eq)

data Identifier = Identifier String deriving (Show, Eq)

data Const = IntConst Int deriving (Show, Eq)
data Statement = ReturnVal Const deriving (Show, Eq)

type FuncBody = [Statement]
data Func = Func ReturnType Identifier FuncBody deriving (Show, Eq)
data Prog = Prog Func deriving (Show, Eq)
