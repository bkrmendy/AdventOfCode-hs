module Year2021.Day10 where

import Challenge

import Data.Either (partitionEithers)
import Data.List (sort)
import qualified Data.Map.Strict as M

opening, closing :: String
opening = "([{<"
closing = ")]}>"

pairs :: M.Map Char Char
pairs  = M.fromList $ zip opening closing

points1 :: M.Map Char Int
points1 = M.fromList $ zip closing [3, 57, 1197, 25137]

points2 :: M.Map Char Int
points2 = M.fromList $ zip closing [1..]

-- | https://github.com/glguy/advent2021/blob/main/execs/Day10.hs
validate :: String -> String -> Either Char String
validate (x:xs) (y:ys) | x == y                       = validate xs ys
validate xs     (y:ys) | Just x <- y `M.lookup` pairs = validate (x:xs) ys
validate _      (y:_ )                                = Left y
validate xs     []                                    = Right xs

partOneI :: [String] -> Int
partOneI is = sum
            $ map (points1 M.!)
            $ fst
            $ partitionEithers (validate [] <$> is)
            
partTwoI :: [String] -> Int
partTwoI is = median
            $ sort
            $ map score 
            $ snd
            $ partitionEithers (validate [] <$> is)
  where score = foldl (\a d -> a * 5 + (points2 M.! d)) 0
        median xs = xs !! (length xs `div` 2)

instance Challenge [String] where
  parse = lines
  partOne = show . partOneI
  partTwo = show . partTwoI