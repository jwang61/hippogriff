module AST where

data ReturnType = IntType
                | CharType
                deriving (Show, Eq)

data Identifier = Identifier { idStr :: String } deriving (Show, Eq)

data Const = IntConst { intVal :: Int } deriving (Show, Eq)
data Statement = ReturnVal { const :: Const } deriving (Show, Eq)

type FuncBody = [Statement]
data Func = Func { returnType :: ReturnType,
                   identifier :: Identifier,
                   funcBody :: FuncBody } deriving (Show, Eq)
data Prog = Prog { func :: Func } deriving (Show, Eq)
