module Parse (parse)  where

import Control.Monad.State (State, get, put, evalState)

import qualified Token as Tok
import qualified AST as AST

type ParserM = State [Tok.Token]

parse :: [Tok.Token] -> AST.Prog
parse = evalState parseProg

parseProg :: ParserM AST.Prog
parseProg = AST.Prog <$> parseFunc

pop :: ParserM (Maybe Tok.Token)
pop = do
  toks <- get
  if null toks
  then return Nothing
  else do
    put $ tail toks
    return $ Just (head toks)

peek :: ParserM (Maybe Tok.Token)
peek = do
  toks <- get
  if null toks
  then return Nothing
  else do
    return $ Just (head toks)

isToken :: Tok.Token -> ParserM Bool
isToken expectedTok = do
  maybeTok <- peek
  case maybeTok of
    Just tok -> return (tok == expectedTok)
    Nothing -> return False

assertNextToken :: Tok.Token -> String -> ParserM ()
assertNextToken expectedTok errMsg = do
  res <- isToken expectedTok
  if res
  then do
    _ <- pop
    return ()
  else error errMsg

parseRetType :: ParserM AST.ReturnType
parseRetType = do
  tok <- pop
  case tok of
    Just Tok.IntKeyword  -> return AST.IntType
    Just Tok.CharKeyword -> return AST.CharType
    _ -> fail "Error: Expected a valid return type"

parseIdentifier :: ParserM AST.Identifier
parseIdentifier = do
  tok <- pop
  case tok of
    Just (Tok.Identifier str) -> return $ AST.Identifier str
    _ -> error "Expected valid identifier format."

parseConst :: ParserM AST.Const
parseConst = do
  tok <- pop
  case tok of
    Just (Tok.IntLiteral val) -> return $ AST.IntConst val
    _ -> error "Expected const format"

parseStatement :: ParserM AST.Statement
parseStatement = do
  tok <- pop
  case tok of
    Just (Tok.ReturnKeyword) -> do
      cons <- parseConst
      assertNextToken Tok.Semicolon "Expected semicolon in statement"
      return . AST.ReturnVal $ cons
    _ -> error ("Unexpected token in statement" ++ show tok)

parseFuncBody :: ParserM AST.FuncBody
parseFuncBody = parseStatements []
  where parseStatements statements = do
          res <- isToken Tok.CloseBrace
          if res
          then pop >>= (\_ -> return statements)
          else do
            statement <- parseStatement
            parseStatements (statements ++ [statement])

parseFunc :: ParserM AST.Func
parseFunc = do
  retType  <- parseRetType
  funcName <- parseIdentifier
  assertNextToken Tok.OpenParenthesis "Expected open parenthesis in func"
  assertNextToken Tok.CloseParenthesis "Expected closed parenthesis in func"
  assertNextToken Tok.OpenBrace "Expected open brace in func"
  body     <- parseFuncBody
  return $ AST.Func retType funcName body
