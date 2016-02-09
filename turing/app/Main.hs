module Main where

import System.Environment
import Lib
import Machine

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    putStr content
