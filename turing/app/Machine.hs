{-# LANGUAGE OverloadedStrings #-}

module Machine where

import Data.Aeson
import Control.Applicative

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
    parseJSON (Object v) = Transition      <$>
                           v .: "read"     <*>
                           v .: "to_state" <*>
                           v .: "write"    <*>
                           v .: "action"
    parseJSON _          = empty
