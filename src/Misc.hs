module Misc where

import           Control.Monad                 (mapM_)
import qualified Data.ByteString.Lazy     as B

import           Control.Lens
import           Data.Aeson                    (decode)
import           Data.Aeson.Encode.Pretty      (encodePretty)

import           Types

charToJSON :: Character -> B.ByteString
charToJSON = encodePretty

charFromJSON :: B.ByteString -> Maybe Character
charFromJSON = decode

charToFile :: Character -> FilePath -> IO ()
charToFile char f = B.writeFile f (encodePretty char)

charsToFile :: [Character] -> FilePath -> ()
charsToFile chars f = mapM_ (flip B.appendFile) (fmap encodePretty chars) f

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
