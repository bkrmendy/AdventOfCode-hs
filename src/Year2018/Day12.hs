{-# LANGUAGE TupleSections #-}
module Year2018.Day12 where
  
import Challenge

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import qualified Debug.Trace as T

type Pot = (Int, Char)
type Patterns = S.Set [Int]

essence :: String -> [Int]
essence = map fst . filter ((== '#'). snd) . zip [0..]

pattern :: String -> Maybe [Int]
pattern s = case splitOn " => " s of
  [p, ('#':_)] -> Just (essence p)
  _      -> Nothing 
  
parseI :: String -> Patterns
parseI = S.fromList . mapMaybe pattern . lines
  
initial :: S.Set Int
initial = S.fromList
        . essence
        $ "#..#.#..##......###...###"

step :: Patterns -> S.Set Int -> S.Set Int
step ps generation = S.fromDistinctAscList
                   . filter go
                   $ [S.findMin generation - 2 .. S.findMax generation + 2]
  where go = undefined

solve :: Int -> Patterns -> Int
solve n ps = sum
           . grow
           $ initial
  where grow = foldl1 (.) $ replicate n (step ps)

instance Challenge Patterns where
  parse = parseI
  partOne = show . solve 20
