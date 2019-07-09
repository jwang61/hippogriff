module Parse (parse)  where

import Control.Monad.State (State, get, put, evalState)

import qualified Token as Tok
import qualified AST as AST

type ParserM = State [Tok.Token]

parse :: [Tok.Token] -> AST.Prog
parse = evalState parseProg

parseProg :: ParserM AST.Prog
parseProg = AST.Prog <$> parseFunc

getToken :: ParserM (Maybe Tok.Token)
getToken = do
  toks <- get
  if null toks
  then return Nothing
  else do
    put $ tail toks
    return $ Just (head toks)

parseRetType :: ParserM AST.ReturnType
parseRetType = do
  tok <- getToken
  case tok of
    Just Tok.IntKeyword  -> return AST.IntType
    Just Tok.CharKeyword -> return AST.CharType
    _ -> fail "Error: Expected a valid return type"

parseIdentifier :: ParserM AST.Identifier
parseIdentifier = do
  tok <- getToken
  case tok of
    Just (Tok.Identifier str) -> return $ AST.Identifier str
    _ -> error "Expected valid identifier format."

-- Temp initial func body
tempFuncBody :: ParserM AST.FuncBody
tempFuncBody = return [AST.ReturnVal . AST.IntConst $ 1]

parseFunc :: ParserM AST.Func
parseFunc = do
  retType  <- parseRetType
  funcName <- parseIdentifier
  body     <- tempFuncBody
  return $ AST.Func retType funcName body
