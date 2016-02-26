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
    (Just crt) -> max 0 $ crt - (critAvoid defender)
    Nothing    -> 0

-- again, no weapon triangle
-- rename "atk"?
-- TODO: finish implementing when other character can't attack
damageDone :: Character -> Character -> Int
damageDone attacker defender = case dmg attacker of
    (Just atk) -> atk - (defender ^. (stats . def))
    Nothing    -> 0

calcExp :: (Character, Character) -> CombatResult -> Int
calcExp (attacker, defender) outcome = 
    case outcome of
        Miss       -> 1
        Hit 0      -> 1
        Hit _      -> hitExp
        Critical _ -> hitExp
        Victory  _ -> (calcExp (attacker, defender) (Hit 1)) + baseExp + 20
    where 
        classBonusA = 1 -- TODO 
        classBonusB = 0 -- TODO
        classPower  = 3 -- TODO
        hitExp      = (31 + ((defender ^. level) + classBonusA) - 
            ((attacker ^. level) + classBonusA)) `div` classPower
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

battle :: (RandomGen g) => (Character, Character) -> g -> BattleResult
battle (char1, char2) gen = case char1 ^. curHP <= 0 of
    True  -> BattleResult { _winner = char2, _loser = char1 }
    False -> case char2 ^. curHP <= 0 of
        True  -> BattleResult { _winner = char1, _loser = char2 }
        False -> battle (newAttacker, newDefender) gen''
    where
        (gen', gen'') =  split gen
        (newDefender, newAttacker) = attackRNG (char1, char2) gen' 

attack :: (Character, Character) -> Int -> Int -> BattleStatus
attack (attacker, target) hitRoll critRoll
    | hitRoll <= hitChance = if critRoll <= critChance
                            then BattleStatus 
                                { _lastRound = Critical $ damage * 3
                                , _lastAttacker = attacker
                                , _lastTarget = curHP -~ (damage * 3) $ target }
                            else BattleStatus
                                { _lastRound = Hit damage
                                , _lastAttacker = attacker
                                , _lastTarget = curHP -~ damage $ target }
    | otherwise            = BattleStatus
                                { _lastRound = Miss
                                , _lastAttacker = attacker
                                , _lastTarget = target }
    where
        hitChance  = hitRate attacker target
        critChance = critRate attacker target
        damage     = damageDone attacker target

fightRound :: (RandomGen g) => 
    (Character, Character) -> -- (first attacker, first target)
    g -> 
    IO BattleStatus -- we're printing messages for debugging
fightRound (char1, char2) gen = case whoDoubles (char1, char2) of
    Just someone -> if char1 ^. name == someone ^. name
                    then undefined
                    else undefined
    Nothing -> do
        -- this is sorta ugly and unsafe but I'm letting it slide because 
        -- we're in the IO monad already.
        let [hitRoll1, critRoll1, hitRoll2, critRoll2] = take 4 $ randomRs (1, 100) gen
        let status1 = attack (char1, char2) hitRoll1 critRoll1
        putStrLn $ prettyPrintStatus status1
        case status1 ^. lastRound of
            Victory _ -> return status1
            _         -> do
                let status2 = attack (status1 ^. lastTarget, status1 ^. lastAttacker) hitRoll2 critRoll2
                putStrLn $ prettyPrintStatus status2
                return status2
        -- if status1 ^. lastRound == Victory
        -- then return status1
        -- else do
        --     let status2 = attack (status1 ^. lastTarget, status1 ^. lastAttacker) hitRoll2 critRoll2
        --     putStrLn $ prettyPrintStatus status2
        --     return status2

fight :: (Character, Character) -> IO BattleStatus
fight (char1, char2) = do
    gen <- getStdGen
    let (char1AtkFirst, _) = random gen :: (Bool, StdGen)
    gen' <- newStdGen
    if char1AtkFirst
    then do
        putStrLn $ char1 ^. name ++ " attacks first!"
        status <- fightRound (char1, char2) gen'
        return undefined
    else do
        putStrLn $ char2 ^. name ++ " attacks first!"
        status <- fightRound (char2, char1) gen'
        return undefined
    return undefined

