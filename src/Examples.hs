module Examples where
import Types

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