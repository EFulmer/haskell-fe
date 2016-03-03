{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Types where
import Control.Lens
import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics

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
    , _rank   :: WpnRank } deriving (Generic, Show)

data BattleResult = BattleResult 
    { _winner :: Character
    , _loser  :: Character } deriving Show

data CombatResult = Start | Miss | Hit Int | Critical Int | 
    Victory Int | CritVictory Int deriving (Show, Eq)

data Battle = Battle
    { _lastRound    :: CombatResult
    , _lastAttacker :: Character
    , _lastTarget   :: Character
    , _expTotals    :: Map.Map String Int } deriving Show

data WeaponType = Physical PhysWeapon 
                | Magical MagWeapon deriving (Eq, Generic, Show)

data PhysWeapon = Sword | Lance | Axe | Bow deriving (Eq, Generic, Read, Show)

data MagWeapon = Light | Dark | Anima deriving (Eq, Generic, Read, Show)

data WpnRank = E | D | C | B | A | S | Prf deriving (Eq, Generic, Read, Show)

instance FromJSON Weapon
instance ToJSON Weapon
instance FromJSON WpnRank
instance ToJSON WpnRank
instance FromJSON WeaponType
instance ToJSON WeaponType
instance FromJSON PhysWeapon
instance ToJSON PhysWeapon
instance FromJSON MagWeapon
instance ToJSON MagWeapon

makeLenses ''Character
makeLenses ''Stats
makeLenses ''Weapon
makeLenses ''Battle
makeLenses ''BattleResult

