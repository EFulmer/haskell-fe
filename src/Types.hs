{-# LANGUAGE TemplateHaskell #-}
module Types where
import Control.Lens

data Character = Character 
    { _name    :: String
    , _klass   :: String
    , _level   :: Int
    , _xp      :: Int
    , _curHP   :: Int -- need better way to represent
    , _stats   :: Stats
    , _growths :: Stats
    , _items   :: [Weapon] } deriving Show

data Stats = Stats 
    { _hp  :: Int
    , _pow :: Int
    , _skl :: Int
    , _spd :: Int
    , _lck :: Int
    , _def :: Int
    , _res :: Int } deriving Show

data Weapon = Weapon 
    { _wpName :: String
    , _kind   :: WeaponType
    , _uses   :: Int
    , _mt     :: Int
    , _hit    :: Int
    , _crit   :: Int
    , _wt     :: Int
    , _rng    :: (Int, Int) -- (minRange, maxRange) 
    , _rank   :: WpnRank } deriving Show

data BattleResult = BattleResult 
    { _winner :: Character
    , _loser  :: Character } deriving Show

data CombatResult = Start | Miss | Hit Int | Critical Int | 
    Victory Int | CritVictory Int deriving (Show, Eq)

data BattleStatus = BattleStatus 
    { _lastRound    :: CombatResult
    , _lastAttacker :: Character
    , _lastTarget   :: Character } deriving Show

data WeaponType = Physical PhysWeapon | Magical MagWeapon deriving (Eq, Show)

data PhysWeapon = Sword | Lance | Axe | Bow deriving (Eq, Read, Show)

data MagWeapon = Light | Dark | Anima deriving (Eq, Read, Show)

data WpnRank = E | D | C | B | A | S | Prf deriving (Eq, Read, Show)

makeLenses ''Character
makeLenses ''Stats
makeLenses ''Weapon
makeLenses ''BattleStatus
makeLenses ''BattleResult

