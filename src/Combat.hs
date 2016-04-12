module Combat where
import Control.Lens
import qualified Data.Map as Map
import System.Random
import Examples
import Misc
import Types

atkSpd :: Character -> Int
atkSpd char
  | cn < wgt  = wgt - cn
  | otherwise = char ^. (stats . spd)
    where 
      cn = char ^. (stats . spd)
      wgt = (char ^. items) !! 0 ^. wt

avoid :: Character -> Int
avoid char = atkSpd char  * 2 + char ^. (stats . lck)

-- Return a Maybe because a character with no weapons can't attack.
-- TODO account for non-weapon items (staffs, etc)
dmg :: Character -> Maybe Int
dmg char = if length (char ^. items) > 0 
  then Just $ char ^. (stats . pow) + (char ^. items) !! 0 ^. mt
  else Nothing

-- as above
critical :: Character -> Maybe Int
critical char = if length (char ^. items) > 0
  then Just $ (char ^. (stats . skl) `div` 2) + (char ^. items) !! 0 ^. crit
  else Nothing

critAvo :: Character -> Int
critAvo char = char ^. (stats . lck)

accuracy :: Character -> Int
accuracy char = (char ^. (stats . skl) * 2) + 
  (char ^. (stats . lck) `div` 2) +
  (char ^. items) !! 0 ^. hit

-- TODO ugly
wta :: Character -> Character -> Maybe Character
wta c1 c2 = 
  case ((c1 ^. items), (c2 ^. items)) of
    ((x:xs), (y:ys)) -> 
      case (x ^. kind) `compare` (y ^. kind) of
        LT -> Just c2
        EQ -> Nothing
        GT -> Just c1
    _ -> Nothing

hitRate :: Character -> Character -> Int
hitRate attacker target = max 0 $ 
  case wta attacker target of
    Nothing   -> (accuracy attacker - avoid target)
    Just char -> if char ^. name == attacker ^. name
                 then (accuracy attacker + 15) - (avoid target)
                 else (accuracy attacker - 15) - (avoid target)

-- TODO have the Nothing propagate through and just skip 
-- the attack of a character who can't.
critRate :: Character -> Character -> Int
critRate attacker target = case critical target of
    (Just crt) -> max 0 $ crt - (critAvo target)
    Nothing    -> 0

-- TODO add in weapon triangle
-- rename "atk"?
damageDone :: Character -> Character -> Int
damageDone attacker target = case dmg attacker of
  (Just atk) -> atk - (target ^. (stats . def))
  Nothing    -> 0

calcExp :: (Character, Character) -> CombatResult -> Int
calcExp (attacker, target) outcome = 
  case outcome of
    Miss       -> 1
    Hit 0      -> 1
    Hit _      -> hitExp
    Critical _ -> hitExp
    Victory  _ -> (calcExp (attacker, target) (Hit 1)) + baseExp + 20
  where 
    classBonusA = 1 -- TODO 
    classBonusB = 0 -- TODO
    classPower  = 3 -- TODO
    hitExp      = (31 + ((target ^. level) + classBonusA) - 
                  ((attacker ^. level) + classBonusA)) `div` classPower
    baseExp     = ((target ^. level) * classPower) + classBonusB -
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

survived :: Character -> Int -> Bool
survived c dmg = dmg < c ^. curHP

attack :: (Character, Character) -> Int -> Int -> Battle
attack (attacker, target) rHit rCrit
  | rHit <= hitChance = if rCrit <= critChance
                        then Battle
                          { _lastRound = if target `survived` critDamage
                                         then CritVictory critDamage 
                                         else Critical critDamage
                          , _lastAttacker = attacker
                          , _lastTarget = curHP -~ (damage * 3) $ target
                          , _expTotals = Map.insert undefined undefined undefined }
                        else Battle
                          { _lastRound = if target `survived` damage
                              then Victory damage 
                              else Hit damage
                          , _lastAttacker = attacker
                          , _lastTarget = curHP -~ damage $ target
                          , _expTotals = Map.insert undefined undefined undefined }
    | otherwise         = Battle
                            { _lastRound = Miss
                            , _lastAttacker = attacker
                            , _lastTarget = target
                            , _expTotals = Map.insert undefined undefined undefined }
    where
        hitChance  = hitRate attacker target
        critChance = critRate attacker target
        damage     = damageDone attacker target
        critDamage = 3 * (damageDone attacker target)

fightRound :: (RandomGen g) => 
  (Character, Character) -> -- (first attacker, first target)
  g -> 
  IO Battle -- we're printing messages for debugging
fightRound (char1, char2) gen = case whoDoubles (char1, char2) of
  Just someone -> do
    let [hitRoll1, critRoll1, hitRoll2, critRoll2, hitRoll3, critRoll3] = take 6 $ randomRs (1, 100) gen
    if char1 ^. name == someone ^. name
    then do
      putStrLn $ char1 ^. name ++ " is fast enough to double attack!"
      let status1 = attack (char1, char2) hitRoll1 critRoll1
      putStrLn $ prettyPrintStatus status1
      case status1 ^. lastRound of
        CritVictory _ -> return status1
        Victory     _ -> return status1
        _             -> do
          let char1' = status1 ^. lastAttacker
          let char2' = status1 ^. lastTarget
          let status2 = attack (char2', char1') hitRoll2 critRoll2
          putStrLn $ prettyPrintStatus status2
          case status2 ^. lastRound of
            CritVictory _ -> return status2
            Victory _     -> return status2
            _             -> do
              let char1'' = status2 ^. lastTarget
              let char2'' = status2 ^. lastAttacker
              let status3 = attack (char1, char2) hitRoll3 critRoll3
              putStrLn $ prettyPrintStatus status3
              return status3
      else do
        putStrLn $ char2 ^. name ++ " is fast enough to double attack!"
        let status1 = attack (char1, char2) hitRoll1 critRoll1
        putStrLn $ prettyPrintStatus status1
        case status1 ^. lastRound of
          CritVictory _ -> return status1
          Victory _     -> return status1
          _             -> do
            let char1' = status1 ^. lastAttacker
            let char2' = status1 ^. lastTarget
            let status2 = attack (char2', char1') hitRoll2 critRoll2
            putStrLn $ prettyPrintStatus status2
            case status2 ^. lastRound of 
              CritVictory _ -> return status2
              Victory _     -> return status2
              _             -> do
                let char1'' = status2 ^. lastTarget
                let char2'' = status2 ^. lastAttacker
                let status3 = attack (char2'', char1'') hitRoll3 critRoll3
                putStrLn $ prettyPrintStatus status3
                return status3
  Nothing -> do
    let [hitRoll1, critRoll1, hitRoll2, critRoll2] = take 4 $ randomRs (1, 100) gen
    let status1 = attack (char1, char2) hitRoll1 critRoll1
    putStrLn $ prettyPrintStatus status1
    case status1 ^. lastRound of
      CritVictory _ -> return status1
      Victory _     -> return status1
      _             -> do
        let status2 = attack (status1 ^. lastTarget, status1 ^. lastAttacker) hitRoll2 critRoll2
        putStrLn $ prettyPrintStatus status2
        return status2

fightLoop :: (RandomGen g) => IO Battle -> g -> Int -> IO Battle
fightLoop status gen x = do
  status' <- status
  case status' ^. lastRound of 
      CritVictory _ -> status
      Victory _     -> status
      _             -> do
        putStrLn $ "Round " ++ show x ++ ":"
        status'' <- fightRound (status' ^. lastAttacker, status' ^. lastTarget) gen
        gen' <- newStdGen
        fightLoop (return status'') gen' (succ x)

startFight :: (RandomGen g) => (Character, Character) -> g -> IO Battle
startFight (attacker, target) gen = fightLoop (return $ initBattleStatus (attacker, target)) gen 1

initBattleStatus :: (Character, Character) -> Battle
initBattleStatus (attacker, target) = Battle
  { _lastRound = Start
  , _lastAttacker = attacker
  , _lastTarget = target 
  , _expTotals = Map.fromList [(attacker ^. name, 0), (target ^. name, 0 )] }

fight :: (Character, Character) -> IO Battle
fight (char1, char2) = do
  gen <- getStdGen
  let (char1AtkFirst, _) = random gen :: (Bool, StdGen)
  gen' <- newStdGen
  if char1AtkFirst
  then do
      putStrLn $ char1 ^. name ++ " attacks first!"
      startFight (char1, char2) gen'
  else do
      putStrLn $ char2 ^. name ++ " attacks first!"
      startFight (char2, char1) gen'

