module Scrape where
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import Data.List.Split
import Data.Maybe (catMaybes)
import System.Environment
import Text.HTML.Scalpel
import Text.Read
import Types

wpnTypes :: [WeaponType]
wpnTypes = [Physical Sword, Physical Lance, Physical Axe, Physical Bow, 
    Magical Anima, Magical Dark, Magical Light]

rootURL :: URL
rootURL = "http://serenesforest.net/blazing-sword/inventory/"

weaponPages :: [String]
weaponPages = ["swords/", "lances/", "axes/", "bows/", 
    "anima-tomes/", "dark-tomes/", "light-tomes/"]

weaponURLs :: [URL]
weaponURLs = fmap (rootURL++) weaponPages

urlsWithTypes :: [(WeaponType, URL)]
urlsWithTypes = zip wpnTypes weaponURLs

parseRange :: String -> Maybe (Int, Int)
parseRange [c] = do
    c' <- readMaybe [c] :: Maybe Int
    return (c', c')
-- parseRange [r1, '~', r2] = do
parseRange (r1:_:r2) = do
    r1' <- readMaybe [r1] :: Maybe Int
    r2' <- readMaybe r2 :: Maybe Int
    return (r1', r2') 

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
        , _rank   = rnk'
        , _uses   = dur'
        , _mt     = might'
        , _hit    = ht'
        , _crit   = crt'
        , _wt     = weight' 
        , _rng    = rang' }
    return wpn
parseWpn _ _ = Nothing

parseWpns :: WeaponType -> [String] -> [Maybe Weapon]
parseWpns wt (s:ss) = if s == ""
                    then (parseWpn wt (take 9 ss)):(parseWpns wt (drop 9 ss))
                    else parseWpns wt ss
parseWpns _ _       = []

writeWeaponsToFile :: [Weapon] -> FilePath -> IO ()
writeWeaponsToFile ws f = mapM_ (B.appendFile f) jsonWs
    where
       jsonWs= map encodePretty ws

parseWpnsFromPage :: (WeaponType, URL) -> IO [Weapon]
parseWpnsFromPage (wpnType, url) = do
    weapons <- scrapeURL url (texts ("tr" // "td"))
    case weapons of 
        Just strs -> return $ catMaybes $ parseWpns wpnType strs
        Nothing   -> return $ []

wpnPageToJSON :: (WeaponType, URL) -> IO ()
wpnPageToJSON wu = do
    wpns <- parseWpnsFromPage wu
    let fName = (last . init) $ splitOn "/" $ snd wu
    writeWeaponsToFile wpns $ "data/weapons/" ++ fName ++ ".json"

allWpnsToJSON :: [(WeaponType, URL)] -> IO ()
allWpnsToJSON = mapM_ wpnPageToJSON

main :: IO ()
main = allWpnsToJSON urlsWithTypes
