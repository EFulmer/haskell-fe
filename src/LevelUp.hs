{-# LANGUAGE Rank2Types #-}
module LevelUp where
import Control.Lens
import System.Random
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
