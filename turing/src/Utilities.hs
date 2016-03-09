module Utilities where

import Data.Maybe
import Data.Either

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither l = maybe (Left l) Right

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = foldr go (return [])
    where go x xs = do x <- f x; if null x then xs else do xs <- xs; return $ x++xs
