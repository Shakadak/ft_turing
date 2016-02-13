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
    let machine = (eitherDecode' $ B.pack content :: Either String Machine)
    putStrLn . show $ machine
    putStrLn . show $ checkMachine <$> machine

