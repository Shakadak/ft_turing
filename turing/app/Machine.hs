module Machine where

data Machine = Machine
    { name          :: String
    , alphabet      :: [Char]
    , blank         :: Char
    , states        :: [String]
    , initial       :: String
    , finals        :: [String]
    , transistions  :: [Transition]
    } deriving Show

data Transition = Transition
    { read      :: Char
    , to_state  :: String
    , write     :: Char
    , action    :: Action
    } deriving Show

data Action = Left | Right
