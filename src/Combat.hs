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
-- TODO: finish implementing when other character can't attack
damageDone :: Character -> Character -> Int
damageDone attacker defender = case dmg attacker of
    (Just atk) -> atk - (defender ^. (stats . def))
    Nothing    -> 0

calcExp :: (Character, Character) -> CombatOutcome -> Int
calcExp (attacker, defender) outcome = 
    case outcome of
        Miss    -> 1
        Tink    -> 1
        Hit     -> (31 + ((defender ^. level) + classBonusA) - 
            ((attacker ^. level) + classBonusA)) `div` classPower
        Victory -> (calcExp (attacker, defender) Hit) + baseExp + 20
    where 
        classBonusA = 1 -- TODO 
        classBonusB = 0 -- TODO
        classPower  = 3 -- TODO
        baseExp     = ((defender ^. level) * classPower) + classBonusB -
            (((attacker ^. level) * classPower) + classBonusB) -- poor name...

-- TODO implement attack speed/con
whoDoubles :: (Character, Character) -> Maybe Character
whoDoubles (char1, char2)
    | spd1 - spd2 >= 4 = Just char1
    | spd2 - spd1 >= 4 = Just char2
    | otherwise        = Nothing
    where
        spd1 = char1 ^. (stats . spd)
        spd2 = char2 ^. (stats . spd)

-- run one half of a turn of combat: one character attacks, the other defends
attackRNG :: (RandomGen g) => (Character, Character) -- (attacker, defender)
    -> g 
    -> (Character, Character) -- in the same order
attackRNG (attacker, defender) gen = let
    [hit, crit] = take 2 $ randomRs (1, 100) gen
    attackHit   = hit <= (hitRate attacker defender)
    atk         = damageDone attacker defender
    critHit     = attackHit && (crit <= critRate attacker defender)
    dmgDone     = (fromEnum attackHit) * atk * (succ (fromEnum critHit)) * 3
    newDefender = curHP -~ dmgDone $ defender
    in
    (attacker, newDefender)

attack :: (Character, Character) -> Int -> Int -> (Character, Character)
attack (attacker, defender) hitRoll critRoll
    | hitRoll <= hitChance = if critRoll <= critChance
                            then (attacker, (curHP -~ (damage * 3)) defender)
                            else (attacker, (curHP -~ damage) defender)
    | otherwise            = (attacker, defender)    
    where
        hitChance  = hitRate attacker defender
        critChance = critRate attacker defender
        damage     = damageDone attacker defender

roundOfBattle :: (RandomGen g) => (Character, Character) -- (1st to attack, 2nd)
    -> g
    -> (Character, Character)
roundOfBattle (char1, char2) gen = case whoDoubles (char1, char2) of
    Just char -> if char ^. name == char1 ^. name
                    -- If first character is doubling:
                    then undefined
                    else undefined
    Nothing   -> let 
                (gen', gen'')    = split gen
                (char1', char2') = attackRNG (char1, char2) gen'
                (char2'', char1'') = attackRNG (char1', char2') gen''
                in
                (char1'', char2'')

battle :: (RandomGen g) => (Character, Character) -> g -> BattleResult
battle (char1, char2) gen = case char1 ^. curHP <= 0 of
    True  -> BattleResult { _winner = char2, _loser = char1 }
    False -> case char2 ^. curHP <= 0 of
        True  -> BattleResult { _winner = char1, _loser = char2 }
        False -> battle (newAttacker, newDefender) gen''
    where
        (gen', gen'') =  split gen
        (newDefender, newAttacker) = attackRNG (char1, char2) gen' 
