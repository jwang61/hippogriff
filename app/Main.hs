module Main where

import qualified Data.Text.IO as IO
import qualified Lex as L
import qualified Parse as P

file :: FilePath
file = "testfile.c"

main :: IO ()
main = do
    contents <- IO.readFile file
    let tokens = L.lex contents
        ast = P.parse tokens
    print tokens
    print ast
