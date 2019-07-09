module Main where

import qualified Data.Text.IO as IO
import qualified Lex as L
--import qualified Token as Tok

file :: FilePath
file = "testfile.c"

main :: IO ()
main = do
    contents <- IO.readFile file
    print . L.lex $ contents
