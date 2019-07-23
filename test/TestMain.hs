module Main where

import Test.HUnit

import LexTest (lexTests)

main :: IO Counts
main = runTestTT lexTests

