{-# LANGUAGE TupleSections #-}

module Year2021.Day5 where

import Challenge
import Utils (int, parseL)
  
import Text.Parsec (Parsec, char, string)
import qualified Data.Map.Strict as M

data Point = MkPoint { _x :: Int, _y :: Int } deriving (Eq, Ord, Show)
data Line = MkLine { _from :: Point, _to :: Point } deriving Show

type Intersections = M.Map Point Int

point :: Parsec String () Point
point = MkPoint <$> (int <* char ',') <*> int

line :: Parsec String () Line
line = MkLine <$> (point <* string " -> ") <*> point

parseI :: String -> [Line]
parseI = parseL line

horizontal, vertical :: Line -> Bool
horizontal (MkLine (MkPoint x1 _) (MkPoint x2 _)) = x1 == x2
vertical (MkLine (MkPoint _ y1) (MkPoint _ y2)) = y1 == y2

ordered :: (Ord a) => a -> a -> (a, a)
ordered a b = if a < b then (a, b) else (b, a)

pointsOnDiagonal :: Point -> Point -> [Point]
pointsOnDiagonal p1 p2 =
  if y1 < y2
    then [MkPoint x (y1 + offset) | (x, offset) <- zip [x1..x2] [0..]]
    else [MkPoint x (y1 - offset) | (x, offset) <- zip [x1..x2] [0..]]
  where (MkPoint x1 y1, MkPoint x2 y2) = ordered p1 p2

pointsOnLine :: Line -> [Point]
pointsOnLine line@(MkLine p1 p2) =
  if horizontal line || vertical line
    then [MkPoint x y | x <- [x1..x2], y <- [y1..y2]]
    else pointsOnDiagonal p1 p2
  where (MkPoint x1 y1, MkPoint x2 y2) = ordered p1 p2

intersections :: [Line] -> Intersections
intersections = M.fromListWith (+)
              . map (, 1)
              . concatMap pointsOnLine

atLeastTwoOverlap :: Intersections -> Int
atLeastTwoOverlap = M.size . M.filter (> 1)

partOneI :: [Line] -> Int
partOneI = atLeastTwoOverlap
         . intersections
         . filter (\l -> horizontal l || vertical l)             

partTwoI :: [Line] -> Int
partTwoI = atLeastTwoOverlap . intersections

instance Challenge [Line] where
  parse = parseI
  partOne = show . partOneI
  partTwo = show . partTwoI


