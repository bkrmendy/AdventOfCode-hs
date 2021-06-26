module Year2020.Day6 where

import Challenge

import Data.List.Split (splitOn)
import Data.List (foldl')
import qualified Data.Set as S

parseI :: String -> [[String]]
parseI = map (splitOn "\n") . splitOn "\n\n"

anyQuestionsYes :: [String] -> Int
anyQuestionsYes = S.size . S.fromList . concat

allQuestionsYes :: [String] -> Int
allQuestionsYes is = S.size $ foldl' S.intersection (head ss) (tail ss)
  where ss = map S.fromList is

solve :: ([String] -> Int) -> [[String]] -> Int
solve f = sum . map f

instance Challenge [[String]] where
  parse = parseI
  partOne = show . solve anyQuestionsYes
  partTwo = show . solve allQuestionsYes
