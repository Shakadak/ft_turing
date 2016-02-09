module Main where

import Prelude hiding (read)
import System.Environment
import Lib
import Machine
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (Prelude.head args)
    putStr content
    putStrLn "-----"
    putStrLn . show $ (decode $ B.pack content :: Maybe Machine)
    putStrLn . B.unpack $ encode test

test = Machine
    { name = "test_unary_sub"
    , alphabet = ['1', '.', '-', '=']
    , blank = '.'
    , states = ["scanright", "eraseone", "subone", "skip", "HALT" ]
    , initial = "scanright"
    , finals = ["HALT"]
    , transitions =
        [ ("scanright", [Transition { read = '.', to_state = "HALT", write = '.', action = "RIGHT" }])]
    }
