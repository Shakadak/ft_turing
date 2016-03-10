module Tape (Tape, left, right, lift, head, write) where
import Prelude hiding (read, head)

data Tape = Tape String Char String

instance Show Tape where
    show (Tape ls h rs) =  "[" ++ l ++ "<" ++ [h] ++ ">" ++ r ++ "]"
        where l = reverse $ take 38 ls
              r = take 37 rs

left, right :: Tape -> Tape
left (Tape (l:ls) h rs)  = Tape ls l (h:rs)
right (Tape ls h (r:rs)) = Tape (h:ls) r rs

lift :: Char -> String -> Tape
lift blank (h:as) = Tape (repeat blank) h (as ++ repeat blank)

head :: Tape -> Char
head (Tape _ a _) = a

write :: Char -> Tape -> Tape
write a (Tape ls _ rs) = Tape ls a rs
