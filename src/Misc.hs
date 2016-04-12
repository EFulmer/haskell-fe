module Misc where

import qualified Data.ByteString.Lazy     as B
import           Control.Lens
import           Data.Aeson.Encode.Pretty      (encodePretty)

import           Types

charToJSON :: Character -> B.ByteString
charToJSON = encodePretty

fightFinished :: Battle -> Bool
fightFinished status = case status ^. lastRound of
  Victory _ -> True
  _         -> False

prettyPrintStatus :: Battle -> String
prettyPrintStatus status = case status ^. lastRound of
  Miss        -> attacker' ++ " missed " ++ target' ++ "!"
  Hit x       -> attacker' ++ " hits " ++ target' ++ " for " ++ show x ++ 
    " HP of damage! " ++ target' ++ " has " ++ targetHP' ++ " HP remaining."
  Critical x  -> attacker' ++ " scores a critical hit on " ++ target' ++ 
    " for " ++ show x ++ " HP of damage! " ++ target' ++ " has " ++ 
    targetHP' ++ " HP remaining."
  Victory x   -> attacker' ++ " hits for " ++ show x ++ 
    " damage, winning the fight!"
  CritVictory x -> attacker' ++ " scores a critical hit on " ++ target' ++
    " for " ++ show x ++ " damage, winning the fight!"
  where
    attacker' = status ^. lastAttacker ^. name
    target'   = status ^. lastTarget ^. name
    targetHP' = show $ status ^. lastTarget ^. curHP
