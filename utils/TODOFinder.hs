module TODOFinder where
import System.Directory (getDirectoryContents)
import System.FilePath  (takeExtension)
import System.IO        (readFile)
import Text.Regex

toDoRE = mkRegex ".*-- TODO (.*)"

haskSrc = filter (\f -> takeExtension f == ".hs")

lineNos ss = zip [1..] ss

main :: IO ()
main = do
  f <- readFile "utils/TODOFinder.hs"
  let f' = matchRegex toDoRE f
  print f'
