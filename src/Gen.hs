module Gen (
  generate
  ) where

import qualified AST

generate :: AST.Prog -> IO ()
generate = genFunc . AST.func

genFunc :: AST.Func -> IO ()
genFunc func = do
  putStrLn $ "        .globl _" <> (AST.idStr $ AST.identifier func)
  putStrLn $ "_" <> (AST.idStr $ AST.identifier func) <> ":"
  genFuncBody $ AST.funcBody func

genFuncBody :: AST.FuncBody -> IO ()
genFuncBody = sequence_ . (map genStatement)

genStatement :: AST.Statement -> IO ()
genStatement (AST.ReturnVal cons) = do
  putStrLn $ "        movl    $" <> (show . AST.intVal $ cons) <> ", %eax"
  putStrLn "        ret"
