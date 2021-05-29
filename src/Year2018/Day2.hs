module Year2018.Day2 where
import Challenge

import Data.List (sort, group)

counts :: String -> [(Int, Char)]
counts = map (\ls -> (length ls, head ls)). group . sort

nOfAny :: Int -> [(Int, Char)] -> Bool
nOfAny n = elem n . map fst

checksum :: [String] -> Int
checksum ids = ns 2 * ns 3
  where
    letters = map counts ids
    ns = \n -> length (filter (nOfAny n) letters)

difference :: String -> String -> Int
difference [] a = length a
difference a [] = length a
difference (x:xs) (y:ys) = (if x == y then 0 else 1) + difference xs ys

common :: String -> String -> String
common [] _ = []
common _ [] = []
common (x:xs) (y:ys) = if x == y then x:common xs ys else common xs ys

near :: [String] -> [String]
near ss = map (uncurry (foldr common)) . filter (not . null . snd) $ map (go ss) ss
  where
    go :: [String] -> String -> (String, [String])
    go xs x = (x, [s | s <- xs, difference x s == 1])

instance Challenge [String] where
  parse = lines
  partOne = show . checksum
  partTwo = head . near