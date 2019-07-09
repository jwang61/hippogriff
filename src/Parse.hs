module Parse where

import qualified Token as Tok
import qualified AST as AST

parse :: [Tok.Token] -> AST.Prog
parse = AST.Prog . parseFunc

parseRetType :: Tok.Token -> AST.ReturnType
parseRetType Tok.IntKeyword = AST.IntType
parseRetType Tok.CharKeyword = AST.CharType
parseRetType _ = error "Expected a valid return type."

parseRetType :: [Tok.Token] -> (AST.ReturnType, [Tok.Token])
parseRetType Tok.IntKeyword:rest = (AST.IntType,rest)
parseRetType Tok.CharKeyword = AST.CharType
parseRetType _ = error "Expected a valid return type."

parseIdentifier :: Tok.Token -> AST.Identifier
parseIdentifier (Tok.Identifier str) = AST.Identifier str
parseIdentifier _ = error "Expected valid identifier format."

parseFunc :: [Tok.Token] -> AST.Func
parseFunc tokens = AST.Func retType funcName body
  where retType = parseRetType $ head tokens
        funcName = parseIdentifier $ head $ tail tokens
        body =


          -- module AST where

          --   data ReturnType = IntType
          --                   | CharType
          --                   deriving (Show, Eq)

          --   data Identifier = Identifier String deriving (Show, Eq)

          --   data Const = IntConst Int deriving (Show, Eq)
          --   data Statement = ReturnVal Const deriving (Show, Eq)

          --   data FuncBody = [Statement]
          --   data Func = Func ReturnType Identifier FuncBody deriving (Show, Eq)
          --   data Prog = Prog Func deriving (Show, Eq)
