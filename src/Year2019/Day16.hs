{-# LANGUAGE FlexibleInstances #-}
module Year2019.Day16 where
import Challenge
import Data.Char (digitToInt)
import Data.Array as Array
import Debug.Trace

-- | Part 1

patterns :: [[Int]]
patterns = [generatePattern n | n <- [1..]]
  where
    basePattern = [0, 1, 0, -1] ++ basePattern
    generatePattern n = drop 1 (concatMap (replicate n) basePattern)

phase :: [Int] -> [Int] -> Int
phase xs ys = (`mod` 10) . abs . sum $ zipWith (*) xs ys

fft :: [Int] -> [[Int]]
fft series = thisPhase:fft thisPhase
  where thisPhase = zipWith phase (replicate (length series) series) patterns

-- | Part 2
-- Solution due to https://github.com/encse/adventofcode/blob/master/2019/Day16/Solution.cs

toInt :: [Int] -> Int
toInt = foldl (\acc i -> acc * 10 + i) 0

fromList :: Int -> [a] -> Array Int a
fromList starting xs = array (starting, starting - 1 + length xs) (zip [starting..] xs)

coeffs :: Integer -> [Integer] -> [Int]
coeffs bij (i:is) = fromInteger (bij `mod` 10) : coeffs next is
  where next = bij * (i + 99) `div` i

bijMods :: Int -> Array Int Int
bijMods len = fromList 1 $ take len $ coeffs 1 [1..]

partTwoI :: [Int] -> [Int]
partTwoI input = [s i `mod` 10 | i <- [1..8]]
  where
    t = toInt (take 7 input)
    xs = fromList 0 input
    ccol = length xs * 10000 - t
    cs = bijMods ccol
    x j = xs ! ((t + j - 1) `mod` length xs)
    coeff i j = cs ! (j - i + 1)
    s i = sum [ x j * coeff i j | j <- [i..ccol]]


instance Challenge [Int] where
  parse = map digitToInt
  partOne = concatMap show . take 8 . last . take 100 . fft -- output: 11833188, somewhat regular but OK
  partTwo = concatMap show . partTwoI                       -- output: 55005000, cannot believe it
