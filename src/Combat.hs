module Combat where
import Control.Lens
import System.Random
import Examples
import Types

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
    newDefender = curHP -~ dmgDone $ defender
    in
    (attacker, newDefender)
