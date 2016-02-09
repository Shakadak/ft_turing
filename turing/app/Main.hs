module Main where

import System.Environment
import Lib
import Machine
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson.Generic

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (Prelude.head args)
    putStr content
    putStrLn "-----"
    putStrLn . show $ (decode $ B.pack content :: Maybe Machine)
