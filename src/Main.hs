{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module Main where
import Control.Lens
import System.Random
import Examples
import Types

levelAStat :: Character -> (Lens' Stats Int) -> Int -> Character
levelAStat char stat roll = 
    if (char ^. (growths . stat)) < roll
    then char
    else (stats . stat) +~ 1 $ char

levelUp :: (RandomGen g) => Character -> g -> Character
levelUp char gen = let
    [rHP, rPow, rSkl, rSpd, rLck, rDef, rRes] = take 7 $ randomRs (1, 100) gen :: [Int]
    lHP  = levelAStat char hp rHP
    lPow = levelAStat lHP pow rPow
    lSkl = levelAStat lPow skl rSkl
    lSpd = levelAStat lSkl spd rSpd
    lLck = levelAStat lSpd lck rLck
    lDef = levelAStat lLck def rDef
    lRes = levelAStat lDef res rRes
    in
    lRes    

avoid :: Character -> Int
avoid char = char ^. (stats . spd) * 2 + char ^. (stats . lck)

-- character with no weapons can't attack
dmg :: Character -> Maybe Int
dmg char = if length (char ^. items) > 0 
    then Just $ char ^. (stats . pow) + (char ^. items) !! 0 ^. mt
    else Nothing

critAvo :: Character -> Int
critAvo char = char ^. (stats . lck)

combat :: (Character, Character) -> (Character, Character)
combat (attacker, defender) = undefined

-- TBD!
main :: IO ()
main = undefined
