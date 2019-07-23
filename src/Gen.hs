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
genFuncBody =  map genStatement

genStatement :: AST.Statement -> T.Text
genStatement (AST.ReturnVal cons) =
  T.pack $ "        movl    $" <> (show . AST.intVal $ cons) <> ", %eax \n\
  \        ret"
