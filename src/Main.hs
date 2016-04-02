module Main where
import Combat
import Examples
import LevelUp
import Misc
import Types

mainMenu = do 
  putStrLn "Welcome to Haskell FE 0.0.1!"
  putStrLn "Please select an option:"
  putStrLn "1. Create a new character."
  putStrLn "2. Battle two existing characters."
  putStrLn "3. View existing characters."
  option <- getLine
  case option of
    "1" -> undefined
    "2" -> undefined
    "3" -> undefined
    _   -> putStrLn ("Unrecognized option: " ++ option) >> mainMenu

-- TBD!
main :: IO ()
main = mainMenu
