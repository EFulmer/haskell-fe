{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module FE where
import Control.Lens
import System.Random

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

data WeaponType = Physical PhysWeapon | Magical MagWeapon deriving (Show, Eq)

data PhysWeapon = Sword | Lance | Axe | Bow deriving (Show, Eq)

data MagWeapon = Light | Dark | Anima deriving (Show, Eq)

makeLenses ''Character
makeLenses ''Stats
makeLenses ''Weapon

rapier = Weapon 
    { _wpName = "Rapier"
    , _kind   = Physical Sword
    , _uses   = 40
    , _mt     = 7
    , _hit    = 95
    , _crit   = 10
    , _rng    = (1, 1) }

maniKatti = Weapon 
    { _wpName = "Mani Katti"
    , _kind   = Physical Sword
    , _uses   = 45
    , _mt     = 8
    , _hit    = 80
    , _crit   = 20
    , _rng    = (1, 1) }

wolfBeil = Weapon 
    { _wpName = "Wolf Beil"
    , _kind   = Physical Axe
    , _uses   = 30
    , _mt     = 10
    , _hit    = 75
    , _crit   = 5
    , _rng    = (1, 1) }

eliwood = Character 
    { _name  = "Eliwood"
    , _klass = "Lord"
    , _level = 1
    , _xp    = 0
    , _curHP = 18
    , _stats = Stats 
        { _hp  = 18
        , _pow = 5
        , _skl = 5
        , _spd = 7
        , _lck = 7
        , _def = 5
        , _res = 0 }
    ,_growths = Stats 
        { _hp  = 80
        , _pow = 45
        , _skl = 50
        , _spd = 50
        , _lck = 45
        , _def = 30
        , _res = 35 },
    _items   = [rapier] }

lyn = Character
    { _name  = "Lyn"
    , _klass = "Lord"
    , _level = 1
    , _xp    = 0 
    , _curHP = 16
    , _stats = Stats
        { _hp  = 16
        , _pow = 4
        , _skl = 7
        , _spd = 9
        , _lck = 5
        , _def = 2
        , _res = 0 }
    , _growths = Stats
        { _hp  = 70
        , _pow = 40
        , _skl = 60
        , _spd = 60
        , _lck = 55
        , _def = 20
        , _res = 30 }
    , _items = [maniKatti] }

hector = Character
    { _name  = "Hector"
    , _klass = "Lord"
    , _level = 1
    , _xp    = 0 
    , _curHP = 19
    , _stats = Stats
        { _hp  = 19
        , _pow = 7
        , _skl = 4
        , _spd = 5
        , _lck = 3
        , _def = 8
        , _res = 0 }
    , _growths = Stats
        { _hp  = 90
        , _pow = 60
        , _skl = 45
        , _spd = 35
        , _lck = 30
        , _def = 50
        , _res = 25 }
    , _items = [wolfBeil] }

levelAStat :: Character -> (Lens' Stats Int) -> Int -> Character
levelAStat char stat roll = 
    if (char ^. (growths . stat)) < roll
    then char
    else (stats . stat) +~ 1 $ char

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

critAvo :: Character -> Int
critAvo char = char ^. (stats . lck)

combat :: (Character, Character) -> (Character, Character)
combat (attacker, defender) = undefined
