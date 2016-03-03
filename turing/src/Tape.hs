module Tape (Tape, left, right, lift, read, write) where
import Prelude hiding (read)

data Tape a = Tape [a] a [a]

left, right :: Tape a -> Tape a
left (Tape (l:ls) h rs)  = Tape ls l (h:rs)
right (Tape ls h (r:rs)) = Tape (h:ls) r rs

lift :: a -> [a] -> Tape a
lift blank (h:as) = Tape (repeat blank) h (as ++ repeat blank)

read :: Tape a -> a
read (Tape _ a _) = a

write :: a -> Tape a -> Tape a
write a (Tape ls _ rs) = Tape ls a rs