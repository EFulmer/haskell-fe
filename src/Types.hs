{-# LANGUAGE TemplateHaskell #-}
module Types where
import Control.Lens

data Character = Character 
    { _name :: String
    , _klass :: String
    , _level :: Int
    , _xp :: Int
    , _curHP :: Int -- need better way to represent
    , _stats :: Stats
    , _growths :: Stats
    , _items :: [Weapon] } deriving Show

data Stats = Stats 
    { _hp :: Int
    , _pow :: Int
    , _skl :: Int
    , _spd :: Int
    , _lck :: Int
    , _def :: Int
    , _res :: Int } deriving Show

data Weapon = Weapon 
    { _wpName :: String
    , _kind :: WeaponType
    , _uses :: Int
    , _mt :: Int
    , _hit :: Int
    , _crit :: Int
    , _rng :: (Int, Int) } deriving Show -- range = (min, max)

data BattleResult = BattleResult 
    { _winner :: Character
    , _loser :: Character } deriving Show

data CombatResult = Miss | Hit Int | Critical Int | 
    Victory Int deriving (Show, Eq)

data BattleStatus = BattleStatus 
    { _lastRound    :: CombatResult
    , _lastAttacker :: Character
    , _lastTarget   :: Character } deriving Show

data WeaponType = Physical PhysWeapon | Magical MagWeapon deriving (Show, Eq)

data PhysWeapon = Sword | Lance | Axe | Bow deriving (Show, Eq)

data MagWeapon = Light | Dark | Anima deriving (Show, Eq)

makeLenses ''Character
makeLenses ''Stats
makeLenses ''Weapon
makeLenses ''BattleStatus
makeLenses ''BattleResult

prettyPrintStatus :: BattleStatus -> String
prettyPrintStatus status = case status ^. lastRound of
    Miss       -> attacker' ++ " missed " ++ target' ++ "!"
    Hit x      -> attacker' ++ " hits " ++ target' ++ " for " ++ show x ++ 
        " HP of damage! " ++ target' ++ " has " ++ targetHP' ++ " HP remaining."
    Critical x -> attacker' ++ " scores a critical hit on " ++ target' ++ 
        " for " ++ show x ++ " HP of damage!" ++ target' ++ " has " ++ 
        targetHP' ++ " HP remaining."
    Victory x  -> attacker' ++ " hits for " ++ show x ++ 
        "damage, winning the fight!"
    where
        attacker' = status ^. lastAttacker ^. name
        target'   = status ^. lastTarget ^. name
        targetHP' = show $ status ^. lastTarget ^. curHP
