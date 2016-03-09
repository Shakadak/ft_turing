{-# LANGUAGE OverloadedStrings #-}

module Machine where

import Prelude hiding (read, head)
import qualified Data.Vector as V (toList)
import qualified Data.HashMap.Strict as HM (lookup, toList)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.List hiding (head)
import Data.Text (Text, unpack)
import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Utilities

import Tape

-- Data manipulation

evaluateAtHead :: (Machine, Tape Char) -> Either String (Machine, Tape Char)
evaluateAtHead (m, tape) = do
    (newState, symbol, action)  <- getTransition m (head tape)
    return (m {initial = newState}, applyTransition symbol action tape)
        where getTransition machine symbol = maybeToEither ("Could not find matching transition for the pair (" ++ initial machine ++ ", " ++ [symbol] ++ ").") $ lookup (initial machine, symbol) (transitions machine)
{-
showTapeMachine :: (Machine, Tape Char) -> String
showTapeMachine (m, t) = show t ++ if hasHalted m then "" else showTransition m t
    where showTransition m t = -}

applyTransition :: Char -> Action -> Tape Char -> Tape Char
applyTransition symbol action = moveHead action . Tape.write symbol

moveHead :: Action -> Tape a -> Tape a
moveHead LEFT = left
moveHead RIGHT = right

hasHalted :: Machine -> Bool
hasHalted m = initial m `elem` finals m

loop :: Either String (Machine, Tape Char) -> Either String (Machine, Tape Char)
loop res@(Right(m, _))
    | hasHalted m = res
    | otherwise   = loop . evaluateAtHead =<< res
loop res = res

-- Data definition

data Machine = Machine
    { name          :: String
    , alphabet      :: [Char]
    , blank         :: Char
    , states        :: [String]
    , initial       :: String
    , finals        :: [String]
    , transitions   :: [Transition]
    } deriving (Show)

type Transition = ((String, Char), (String, Char, Action))

from_state :: Transition -> String
read       :: Transition -> Char
to_state   :: Transition -> String
write      :: Transition -> Char
-- action     :: Transition -> Action
from_state ((f_s, _), _) = f_s
read ((_, r), _) = r
to_state (_, (t_s, _, _)) = t_s
write (_, (_, w, _)) = w
-- action (_, (_, _, a)) = a

data JSONTransition = JSONTransition
    { read_      :: Char    -- read
    , to_state_  :: String    -- to_state
    , write_     :: Char     -- write
    , action_    :: Action    -- action
    } deriving (Show)

data Action = LEFT | RIGHT
    deriving (Show, Eq)

-- Parsing

instance FromJSON JSONTransition where
    parseJSON (Object v) = JSONTransition  <$>
                           v .: "read"     <*>
                           v .: "to_state" <*>
                           v .: "write"    <*>
                           lookupAndParse (withText "action" parseAction) "action" v
    parseJSON _          = empty

instance FromJSON Machine where
    parseJSON (Object v) = Machine          <$>
                           v .: "name"      <*>
                           lookupAndParse (withArray "alphabet" (mapM parseJSON . V.toList)) "alphabet" v  <*>
                           v .: "blank"     <*>
                           v .: "states"    <*>
                           v .: "initial"   <*>
                           v .: "finals"    <*>
                           lookupAndParse (withObject "transitions" ((concatMapM parseTransition) . extractTransitions)) "transitions" v
    parseJSON _          = empty

parseTransition :: (String, Value) -> Parser [Transition]
parseTransition (name, arr) = fmap (\x -> ((name, read_ x), (to_state_ x, write_ x, action_ x))) <$> go arr
    where go = withArray name (mapM parseJSON . V.toList) :: Value -> Parser [JSONTransition]

extractTransitions :: Object -> [(String, Value)]
extractTransitions = map (first unpack) . HM.toList

parseAction :: Text -> Parser Action
parseAction "LEFT"  = pure LEFT
parseAction "RIGHT" = pure RIGHT
parseAction txt     = fail $ "key \"action\" expected either \"LEFT\" or \"RIGHT\" value instead of " ++ unpack txt

lookupAndParse :: (Value -> Parser a) -> Text -> Object -> Parser a
lookupAndParse f key obj = case HM.lookup key obj of
              Nothing   -> fail $ "key " ++ show key ++ " not present"
              Just val  -> f val

-- Verification

checkMachine :: Machine -> Bool
checkMachine m = blank m `elem` alphabet m
              && initial m `elem` states m
              && null (finals m \\ states m)
              && all (checkTransition m) (transitions m)

checkTransition :: Machine -> Transition -> Bool
checkTransition m t = from_state t `elem` states m
                   && Machine.read t  `elem` alphabet m
                   && to_state t      `elem` states m
                   && Machine.write t `elem` alphabet m

checkInput :: Machine -> String -> Bool
checkInput m = all (`elem` alphabet m)
