module Main where

import qualified Data.Text.IO as IO
import System.Environment

import qualified Gen as G
import qualified Lex as L
import qualified Parse as P

main :: IO ()
main = do
    [inputF, outputF] <- getArgs
    contents <- IO.readFile inputF
    let tokens = L.lex contents
        ast = P.parse tokens
    G.generate outputF ast
