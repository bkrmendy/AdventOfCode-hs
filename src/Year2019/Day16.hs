{-# LANGUAGE FlexibleInstances #-}
module Year2019.Day16 where
import Challenge
import Data.Char (digitToInt)

patterns :: [[Int]]
patterns = [generatePattern n | n <- [1..]]
  where
    basePattern = [0, 1, 0, -1] ++ basePattern
    generatePattern n = drop 1 (concatMap (replicate n) basePattern)

phase :: [Int] -> [Int] -> Int
phase = (`mod` 10) . abs . sum . zipWith (*)

fft :: [Int] -> [[Int]]
fft series = thisPhase:fft thisPhase
  where
    thisPhase = zipWith phase (replicate (length series) series) patterns

partTwoI :: [String] -> [Int]
partTwoI input = undefined

instance Challenge [Int] where
  parse = map digitToInt
  partOne = concatMap show . take 8 . last . take 100 . fft
  partTwo = show . length
