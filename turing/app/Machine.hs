{-# LANGUAGE OverloadedStrings #-}

module Machine where

import GHC.Generics
import Data.Aeson

data Machine = Machine
    { name          :: String
    , alphabet      :: [Char]
    , blank         :: Char
    , states        :: [String]
    , initial       :: String
    , finals        :: [String]
    , transitions  :: [(String, [Transition])]
    } deriving (Show)

data Transition = Transition
    { read      :: Char
    , to_state  :: String
    , write     :: Char
    , action    :: String
    } deriving (Show)

instance FromJSON Transition where
    parseJSON (Object v) = Transition

instance FromJSON Machine
