module Main where

import Prelude hiding (read)
import System.Environment
import Tape (lift)
import Machine
import Data.List (unfoldr)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Data.Aeson

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
       then putStr usage
       else do
           content <- readFile (head args)
           let input = args!!1
           let res = do machine <- eitherDecodeStrict . encodeUtf8 . pack $ content :: Either String Machine
                        if not $ checkMachine machine
                           then Left "Invalid machine description"
                           else if not $ checkInput machine input
                           then Left "Invalid input compared to machine."
                           else return $ show machine ++ unlines (unfoldr (fmap compute) (return (machine, lift (blank machine) input)))
           either putStrLn putStrLn res

usage :: String
usage = "usage: ft_turing [-h] jsonfile input\n\
        \positional arguments:\n\
        \  jsonfile              json description of the machine\n\
        \\n\
        \  input                 input of the machine\n\
        \optional arguments:\n\
        \  -h, --help            show this help message and exit\n"
