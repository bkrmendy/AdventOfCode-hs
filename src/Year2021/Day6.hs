module Year2021.Day6 where

import Utils (readInt)
import Challenge
  
import Data.List.Split (splitOn)
import Data.Array.ST
import Data.Array.Unboxed

import Control.Monad (forM_)

type School = [Int]

parseI :: String -> School
parseI ns = [ count n school | n <- [0..8]]
  where school = map readInt . splitOn "," $ ns
        count x = length . filter (== x)
        
-- | based on https://github.com/encse/adventofcode/blob/master/2021/Day06/Solution.cs    
runLanternFish :: Int -> School -> Int
runLanternFish days school = sum $ elems $ runSTUArray $ do
  fish <- newListArray (0, 8) school
  forM_ [0 .. days - 1] $ \day -> do
    elemA <- readArray fish (day `mod` 9)
    elemB <- readArray fish ((day + 7) `mod` 9)
    writeArray fish ((day + 7) `mod` 9) (elemA + elemB)
  return fish

instance Challenge School where
  parse = parseI
  partOne = show . runLanternFish 80
  partTwo = show . runLanternFish 256
