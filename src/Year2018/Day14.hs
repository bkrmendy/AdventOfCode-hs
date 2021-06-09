-- | from https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day14.hs
module Year2018.Day14 where

import Challenge
import Utils (readInt)

import Data.List (isPrefixOf, tails)
import Data.Char (digitToInt)
import qualified Data.Sequence as S

digits :: Int -> [Int]
digits i = case i `divMod` 10 of
  (0, y) -> [y]
  (x, y) -> [x, y]

recipes :: [Int]
recipes = 3 : 7 : go 0 1 (S.fromList [3, 7])
  where
    go :: Int -> Int -> S.Seq Int -> [Int]
    go p1 p2 tp = newDigits ++ go p1' p2' tp'
      where
        sc1 = tp `S.index` p1
        sc2 = tp `S.index` p2
        newDigits = digits (sc1 + sc2)
        tp' = tp <> S.fromList newDigits
        p1' = (p1 + sc1 + 1) `mod` S.length tp'
        p2' = (p2 + sc2 + 1) `mod` S.length tp'

pt2 :: String -> Int
pt2 n = (length . takeWhile (not . (ns `isPrefixOf`)) . tails) recipes
  where
    ns = map digitToInt n

instance Challenge String where
  parse = id
  partOne = concatMap show . take 10 . (`drop` recipes) . readInt
  partTwo = show . pt2


