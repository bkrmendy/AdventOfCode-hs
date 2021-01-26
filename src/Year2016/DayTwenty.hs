{-# LANGUAGE FlexibleInstances #-}

module DayTwenty where
import Challenge
import Data.List (sortOn, find)
import Text.Parsec hiding (lower, upper, count)
import Utils

parseInterval :: Parsec String () (Integer, Integer)
parseInterval = (,) <$> integer <*>  (char '-' *> integer)

-- ^ precondition: intervals are sorted ascending by their lower bound
mergeAll :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeAll [a] = [a]
mergeAll (a@(lo, hi):b@(lo', hi'):rest)
  | lo' < hi = mergeAll ((lo, max hi hi'):rest)
  | otherwise = a : mergeAll (b:rest)

pairs :: [(Integer, Integer)] -> [((Integer, Integer), (Integer, Integer))]
pairs = s zip tail . mergeAll . sortOn fst

partOneI :: [(Integer, Integer)] -> Integer
partOneI = pred . fst . snd . unsafeFromMaybe . find (\(a, b) -> snd a < fst b - 1) . pairs

count :: [(Integer, Integer)] -> Integer
count [(_, hi)] = 4294967295 - hi
count ((_, hi):(lo, hi'):rest) = (lo - hi - 1) + count ((lo, hi'):rest)

partTwoI :: [(Integer, Integer)] -> Integer
partTwoI = count . mergeAll . sortOn fst

instance Challenge [(Integer, Integer)] where
  parse = parseLines (sepBy1 parseInterval newline)
  partOne = show . partOneI
  partTwo = show . partTwoI