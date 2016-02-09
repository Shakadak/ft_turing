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
    putStrLn . show $ (eitherDecode $ B.pack content :: Either String Machine)

