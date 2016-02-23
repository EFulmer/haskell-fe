module RNG where
import System.Random

range :: (Int, Int)
range = (1, 100)

requestRollsGen :: (RandomGen g) => g -> Int -> [Int]
requestRollsGen gen n = take n $ randomRs range gen
