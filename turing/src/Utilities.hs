module Utilities where

import Data.Maybe
import Data.Either

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither l = maybe (Left l) Right
