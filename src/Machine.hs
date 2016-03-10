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

compute :: (Machine, Tape) -> (String, Maybe (Machine, Tape))
compute (machine, tape) = (show tape ++ " " ++ trStr, next)
    where tr = getTransition machine (head tape)
          trStr = either (\x -> if ($ machine) hasHalted {- Reads like a fine wine, though I do not like alcohol -} then "Halted: (" ++ initial machine ++ ")" else x) (showTransition . (,) (initial machine, head tape)) tr
          next = either (const Nothing) (\(nextState, symbol, action) -> Just (machine {initial = nextState}, applyTransition symbol action tape)) tr

getTransition :: Machine -> Char -> Either String (String, Char, Action)
getTransition machine symbol = maybeToEither ("Could not find matching transition for the pair (" ++ initial machine ++ ", " ++ [symbol] ++ ").") $ lookup (initial machine, symbol) (transitions machine)

showTapeTransition :: Transition -> Tape -> String
showTapeTransition tr t = show t ++ " " ++ showTransition tr

applyTransition :: Char -> Action -> Tape -> Tape
applyTransition symbol action = moveHead action . Tape.write symbol

moveHead :: Action -> Tape -> Tape
moveHead LEFT = left
moveHead RIGHT = right

hasHalted :: Machine -> Bool
hasHalted m = initial m `elem` finals m

-- Data definition

data Machine = Machine
    { name          :: String
    , alphabet      :: [Char]
    , blank         :: Char
    , states        :: [String]
    , initial       :: String
    , finals        :: [String]
    , transitions   :: [Transition]
    }

instance Show Machine where
    show machine = unlines $ [replicate 80 '*'
                          , "*" ++ filler ++ nameMachine ++ filler ++ "*"
                          , replicate 80 '*'
                          , "Alphabet: [" ++ intercalate ", " (map show $ alphabet machine) ++ "]"
                          , "States  : [" ++ intercalate ", " (states machine) ++ "]"
                          , "Initial : " ++ initial machine
                          , "Finals  : [" ++ intercalate ", " (finals machine) ++ "]"]
                         ++ map showTransition (transitions machine)
                         ++ [replicate 80 '*']
                    where nameMachine = name machine ++ if length (name machine) `mod` 2 == 0 then "" else " "
                          filler = replicate (39 - (length nameMachine `div` 2)) ' '

type Transition = ((String, Char), (String, Char, Action))

showTransition t = "(" ++ from_state t ++ ", " ++ [read t] ++ ") -> (" ++ to_state t ++ ", " ++ [Machine.write t] ++ ", " ++ show (action t) ++ ")"

from_state :: Transition -> String
read       :: Transition -> Char
to_state   :: Transition -> String
write      :: Transition -> Char
action     :: Transition -> Action
from_state ((f_s, _), _) = f_s
read ((_, r), _) = r
to_state (_, (t_s, _, _)) = t_s
write (_, (_, w, _)) = w
action (_, (_, _, a)) = a

data JSONTransition = JSONTransition
    { read_      :: Char    -- read
    , to_state_  :: String  -- to_state
    , write_     :: Char    -- write
    , action_    :: Action  -- action
    }

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
                           lookupAndParse (withObject "transitions" (concatMapM parseTransition . extractTransitions)) "transitions" v
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
