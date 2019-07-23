{-# LANGUAGE OverloadedStrings #-}

module Gen (
  generate
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified AST

generate :: FilePath -> AST.Prog -> IO ()
generate fileName prog = do
  IO.writeFile fileName $ T.unlines $ genProg prog

genProg :: AST.Prog -> [T.Text]
genProg = genFunc . AST.func

genFunc :: AST.Func -> [T.Text]
genFunc func =
  [ "        .globl _" <> (AST.idStr $ AST.identifier func)
  , "_" <> (AST.idStr $ AST.identifier func) <> ":"
  ] ++ (genFuncBody $ AST.funcBody func)

genFuncBody :: AST.FuncBody -> [T.Text]
genFuncBody = map (T.unlines . genStatement)

genExpression :: AST.Exp -> [T.Text]
genExpression (AST.Const val) =
  [T.pack $ "        movl    $" <> (show . AST.intVal $ val) <> ", %eax"]
genExpression (AST.UnopExp op exp) =
  (genExpression exp) ++ [ instructions ]
  where
    instructions = case op of
      AST.Negation -> "        neg     %eax"
      AST.BitwiseComplement -> "        not     %eax"
      AST.LogicalNegation ->
        "        cmpl   $0, %eax\n\
        \        movl   $0, %eax\n\
        \        sete   %al"

genStatement :: AST.Statement -> [T.Text]
genStatement (AST.ReturnVal exps) =
  (genExpression exps) ++ ["        ret"]
