module Scrape where
import Data.List.Split
import Data.Maybe (catMaybes)
import System.Environment
import Text.HTML.Scalpel
import Text.Read
import Types

rootURL :: URL
rootURL = "http://serenesforest.net/blazing-sword/inventory/"

weaponPages :: [String]
weaponPages = ["swords/", "lances/", "axes/", "bows/", 
    "anima-tomes/", "dark-tomes/", "light-tomes/"]

wpnTypes :: [WeaponType]
wpnTypes = [Physical Sword, Physical Lance, Physical Axe, Physical Bow, 
    Magical Anima, Magical Dark, Magical Light]

weaponURLs :: [URL]
weaponURLs = fmap (rootURL++) weaponPages

printWeapons :: URL -> IO ()
printWeapons url = do
    images <- scrapeURL url (texts ("tr" // "td"))
    case images of 
        Just strs -> mapM_ putStrLn strs
        Nothing   -> putStrLn "errored"

urlsWithTypes :: [(WeaponType, URL)]
urlsWithTypes = zip wpnTypes weaponURLs

parseWeaponsFromPage :: (WeaponType, URL) -> IO [Weapon]
parseWeaponsFromPage (wpnType, url) = do
    images <- scrapeURL url (texts ("tr" // "td"))
    case images of 
        Just strs -> return $ catMaybes $ parseWpns wpnType strs
        Nothing   -> return $ []

parseWpns :: WeaponType -> [String] -> [Maybe Weapon]
parseWpns wt (s:ss) = if s == ""
                    then (parseWpn wt (take 9 ss)):(parseWpns wt (drop 9 ss))
                    else parseWpns wt ss
parseWpns _ _       = []

parseWpn :: WeaponType -> [String] -> Maybe Weapon
parseWpn wpN [nam, rnk, rang, weight, might, ht, crt, dur, i] = do
    rnk'    <- (readMaybe rnk :: Maybe WpnRank)
    weight' <- readMaybe weight :: Maybe Int
    might'  <- readMaybe might :: Maybe Int
    ht'     <- readMaybe ht :: Maybe Int
    crt'    <- readMaybe crt :: Maybe Int
    rang'   <- parseRange rang
    dur'    <- readMaybe dur :: Maybe Int
    let wpn = Weapon { _wpName = nam
        , _kind   = wpN
        , _uses   = dur'
        , _rank   = rnk'
        , _mt     = might'
        , _hit    = ht'
        , _crit   = crt'
        , _wt     = weight' 
        , _rng    = rang' }
    return wpn
parseWpn _ _ = Nothing

parseRange :: String -> Maybe (Int, Int)
parseRange [c] = do
    c' <- readMaybe [c] :: Maybe Int
    return (c', c')
parseRange [r1, '~', r2] = do
    r1' <- readMaybe [r1] :: Maybe Int
    r2' <- readMaybe [r2] :: Maybe Int
    return (r1', r2') 

main :: IO ()
main = getArgs >>= handleArgs'

handleArgs' :: [String] -> IO ()
handleArgs' [url] = printWeapons url
handleArgs' _     = putStrLn "usage: list-all-images URL"
