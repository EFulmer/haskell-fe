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

calcExp :: (Character, Character) -> CombatOutcome -> Int
calcExp (attacker, defender) outcome = 
    case outcome of
        Miss    -> 1
        Hit     -> (31 + ((defender ^. level) + classBonusA) - 
            ((attacker ^. level) + classBonusA)) `div` classPower
        Victory -> 0 
    where 
        classBonusA = 0 -- TODO 
        classBonusB = 0 -- TODO
        classPower  = 3 -- TODO

-- run one half of a turn of combat: one character attacks, the other defends
battleRound :: (RandomGen g) => (Character, Character) -- (attacker, defender)
    -> g 
    -> (Character, Character)
battleRound (attacker, defender) gen = let
    [hit, crit] = take 2 $ randomRs (1, 100) gen
    attackHit   = hit <= (hitRate attacker defender)
    atk         = damageDone attacker defender
    critHit     = attackHit && (crit <= critRate attacker defender)
    dmgDone     = (fromEnum attackHit) * atk * (succ (fromEnum critHit)) * 3
    newDefender = curHP -~ dmgDone $ defender
    in
    (attacker, newDefender)

battle :: (RandomGen g) => (Character, Character) -> g -> BattleResult
battle = undefined
