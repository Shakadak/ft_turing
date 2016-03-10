module Utilities where

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither l = maybe (Left l) Right

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = foldr go (return [])
    where go x xs = do y <- f x; if null y then xs else do ys <- xs; return $ y++ys
