{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Machine where

import Data.Typeable
import Data.Data

data Machine = Machine
    { name          :: String
    , alphabet      :: [Char]
    , blank         :: Char
    , states        :: [String]
    , initial       :: String
    , finals        :: [String]
    , transistions  :: [Transition]
    } deriving (Data, Typeable, Show)

data Transition = Transition
    { read      :: Char
    , to_state  :: String
    , write     :: Char
    , action    :: Action
    } deriving (Data, Typeable, Show)

data Action = LEFT | RIGHT deriving (Data, Typeable, Show)
