module Main where

import Prelude hiding (read)
import System.Environment
import Tape
import Machine
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
       then putStr usage
       else do
           content <- readFile (args!!0)
           let input = args!!1
           let machine = (eitherDecode' $ B.pack content :: Either String Machine)
           putStrLn . show $ machine
           putStrLn . show $ checkMachine <$> machine
           putStrLn . show $ checkInput <$> machine <*> pure input

usage = "usage: ft_turing [-h] jsonfile input\n\
\positional arguments:\n\
\  jsonfile              json description of the machine\n\n\
\  input                 input of the machine\n\
\optional arguments:\n\
\  -h, --help            show this help message and exit\n"

test :: Tape Int
test = lift 0 [1]
