module Main where
import Text.Read (readMaybe)
import Combat
import Examples
import LevelUp
import Misc
import Types

-- TODO this may be better represented as a monad transformer; 
-- perhaps IO (Maybe/Either Character)
newChar :: IO Character
newChar = do
  putStr "Name: "
  name <- getLine
  putStr "HP: "
  rHP <- getLine
  putStr "Str: "
  rStr <- getLine
  putStr "Skl: "
  rSkl <- getLine
  putStr "Spd: "
  rSpd <- getLine
  putStr "Lck: "
  rLck <- getLine
  putStr "Def: "
  rDef <- getLine
  putStr "Res: "
  rRes <- getLine
  undefined

mainMenu :: IO ()
mainMenu = do 
  putStrLn "Welcome to Haskell FE 0.0.1!"
  putStrLn "Please select an option:"
  putStrLn "1. Create a new character."
  putStrLn "2. Battle two existing characters."
  putStrLn "3. View existing characters."
  putStr   "Choose: "
  option <- getLine
  case option of
    "1" -> undefined
    "2" -> undefined
    "3" -> undefined
    _   -> putStrLn ("Unrecognized option: " ++ option) >> mainMenu

main :: IO ()
main = mainMenu
