module TODOFinder where
import Control.Monad    (liftM)
import Data.Maybe       (catMaybes)
import System.Directory (getDirectoryContents)
import System.FilePath  (takeExtension)
import System.IO        (readFile)
import Text.Regex

toDoRE = mkRegex ".*-- TODO (.*)"

haskSrc = filter (\f -> takeExtension f == ".hs")

lineNos ss = zip [1..] (lines ss)

fLineNos :: FilePath -> IO [(FilePath, Int, String)]
fLineNos f = do
  c <- liftM lines $ readFile f
  return $ zip3 (repeat f) [1..] c

showTODO :: (FilePath, Int, String) -> String
showTODO (x, y, z) = z ++ " (" ++ x ++ ", line " ++ (show y) ++ ")"

showTODOs :: [(FilePath, Int, String)] -> [String]
showTODOs todos = fmap showTODO todos

printTODOs todos = mapM_ putStrLn $ showTODOs todos

main :: IO ()
main = do
  f <- getDirectoryContents "src"
  let hs = haskSrc $ fmap ("src/"++) f
  print hs
  contents <- liftM (fmap lines) $ mapM readFile hs
  contents2 <- mapM fLineNos hs
  let a = concat contents2
  printTODOs $ catMaybes $ fmap (\(x, y, z) -> case matchRegex toDoRE z of
    Just m -> Just (x, y, concat m)
    Nothing -> Nothing) a
  return ()
