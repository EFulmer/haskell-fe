{-# LANGUAGE Rank2Types #-}
module Main where
import Control.Lens
import System.Random
import Examples
import Types

-- need Rank2Types for the Lens' in the type signature, which needs to be 
-- provided
levelAStat :: Character -> (Lens' Stats Int) -> Int -> Character
levelAStat char stat roll = 
    if (char ^. (growths . stat)) <= roll
    then char
    else (stats . stat) +~ 1 $ char

-- kinda inelegant but whatever... cleaner than 
-- (levelAStat (levelAStat char hp rHP) pow rPow) ...
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

critical :: Character -> Maybe Int
critical char = if length (char ^. items) > 0
    then Just $ (char ^. (stats . skl) `div` 2) + (char ^. items) !! 0 ^. crit
    else Nothing

critAvoid :: Character -> Int
critAvoid char = char ^. (stats . lck)

accuracy :: Character -> Int
accuracy char = (char ^. (stats . skl) * 2) + 
    (char ^. (stats . lck) `div` 2) +
    (char ^. items) !! 0 ^. hit

-- No weapon triangle yet
hitRate :: Character -> Character -> Int
hitRate attacker defender = max (accuracy attacker - avoid defender) 0

critRate :: Character -> Character -> Int
critRate attacker defender = case critical attacker of
    (Just crt) -> crt - (critAvoid defender)
    Nothing    -> 0

-- again, no weapon triangle
-- rename "atk"?
damageDone :: Character -> Character -> Int
damageDone attacker defender = case dmg attacker of
    (Just atk) -> atk - (defender ^. (stats . def))
    Nothing    -> 0

-- run one "phase" of one turn of combat
combat :: (RandomGen g) => (Character, Character) -- (attacker, defender)
    -> g 
    -> (Character, Character)
combat (attacker, defender) gen = let
    [hit, crit] = take 2 $ randomRs (1, 100) gen
    attackHit   = hit <= (hitRate attacker defender)
    atk         = damageDone attacker defender
    critHit     = attackHit && (crit <= critRate attacker defender)
    dmgDone     = (fromEnum attackHit) * atk * (succ (fromEnum critHit)) * 3
    in
    undefined

-- TBD!
main :: IO ()
main = undefined
