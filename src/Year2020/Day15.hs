module Year2020.Day15 where

import Prelude hiding ((++))
    
import Challenge
import Utils (readInt)

import Data.List.Split (splitOn)

import Data.Sequence ((><), index, ViewR(..))
import qualified Data.Sequence as S

import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy

type Starting = S.Seq Int
type Numbers = M.Map Int Int

numbers :: String -> [Int]
numbers = map readInt . splitOn ","

starting :: [Int] -> Numbers
starting xs = M.fromList (zip xs [1..])

game2 :: Int -> Int -> S.Seq Int -> Numbers -> S.Seq Int
game2 upTo recent ns seen
  | turn > upTo = ns
  | otherwise   = game2 upTo next (ns >< S.singleton next) (M.insert recent turn seen)
  where turn = S.length ns
        next = case M.lookup recent seen of
                  Nothing -> 0
                  Just pr -> turn - pr
  
solve :: Int -> [Int] -> Int
solve upTo xs = spoken `index` (upTo - 1)
  where ss = S.fromList xs
        (_ :> lastSpoken) = S.viewr ss
        spoken = game2 upTo lastSpoken ss (starting xs)
    
instance Challenge [Int] where
  parse = numbers
  partOne = show . solve 2020
  partTwo = show . solve 30000000
