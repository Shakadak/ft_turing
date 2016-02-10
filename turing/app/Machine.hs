{-# LANGUAGE OverloadedStrings #-}

module Machine where

import qualified Data.Vector as V (toList)
import qualified Data.HashMap.Strict as HM (lookup, toList)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text, unpack)
import Control.Applicative
import Control.Arrow

data Machine = Machine
    { name          :: String
    , alphabet      :: [Char]
    , blank         :: Char
    , states        :: [String]
    , initial       :: String
    , finals        :: [String]
    , transitions   :: [(String, [Transition])]
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

instance FromJSON Machine where
    parseJSON (Object v) = Machine          <$>
                           v .: "name"      <*>
                           lookupAndParse (withArray "alphabet" (mapM parseJSON . V.toList :: Array -> Parser [Char])) "alphabet" v  <*>
                           v .: "blank"     <*>
                           v .: "states"    <*>
                           v .: "initial"   <*>
                           v .: "finals"    <*>
                           lookupAndParse (withObject "transitions" (mapM parseTransition . extractTransition)) "transitions" v
    parseJSON _          = empty

parseTransition :: (String, Value) -> Parser (String, [Transition])
parseTransition (name, arr) = withArray name (mapM parseJSON . V.toList :: Array -> Parser [Transition]) arr >>= (\xs -> return (name, xs))

extractTransition :: Object -> [(String, Value)]
extractTransition = map (first unpack) . HM.toList

lookupAndParse :: (Value -> Parser a) -> Text -> Object -> Parser a
lookupAndParse f key obj = case HM.lookup key obj of
              Nothing   -> fail $ "key " ++ show key ++ " not present"
              Just val  -> f val
