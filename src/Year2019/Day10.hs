{-# LANGUAGE FlexibleInstances #-}
module Year2019.Day10 where
import Challenge
import Utils (toInt)
import Data.List (sortOn, groupBy)
import qualified Data.Set as Set

type Grid = Set.Set (Float, Float)

slope :: (Float, Float) -> (Float, Float) -> Float
slope (r1, c1) (r2, c2) = pi - atan2 (r1 - r2) (c1 - c2)

distance :: (Float, Float) -> (Float, Float) -> Float
distance (a, b) (x, y) = sqrt ((a - x) ^ 2 + (b - y) ^ 2)

stationCandidates :: Grid -> [(Int, (Float, Float))]
stationCandidates asteroidCoords = [(nAsteroidsVisible c, c) | c <- Set.elems asteroidCoords]
  where
    nAsteroidsVisible :: (Float, Float) -> Int
    nAsteroidsVisible coord = Set.size $ Set.fromList [slope crd coord | crd <- Set.elems asteroidCoords
                                                                       , crd /= coord]

partTwoI :: Grid -> (Float, Float)
partTwoI grid = fire 200 classes
  where
    (_, station) = head (stationCandidates grid)
    slopes = groupBy (\a b -> direction station a == direction station b) [c | c <- Set.elems grid, c /= station]
    classes = sortOn (distance station) <$> sortOn (slope station . head) slopes


fire :: Int -> [[(Float, Float)]] -> (Float, Float)
fire i cs
  | i <= length cs = head $ cs !! (i - 1)
  | otherwise      = fire (i - length cs) $ filter (not . null) $ fmap tail cs

direction :: (Float, Float) -> (Float, Float) -> (Float, Float)
direction (x, y) (a, b) =
  let
    dx = toInt $ a - x
    dy = toInt $ b - y
    g  = gcd dx dy
  in (fromIntegral (dx `div` g), fromIntegral (dy `div` g))

fromLines :: [String] -> Grid
fromLines rows = Set.fromList $ [(fromIntegral row :: Float, fromIntegral col :: Float) | row <- [0..gridHeight]
                                                                                        , col <- [0..gridWidth]
                                                                                        , (rows !! row) !! col == '#'
                                                                                        ]
  where
      gridWidth = length rows - 1
      gridHeight = length (head rows) - 1

fromCoord :: (Float, Float) -> Float
fromCoord (r, c) = r * 100 + c

instance Challenge Grid where
  parse = fromLines . lines
  partOne = show . fst . maximum . stationCandidates
  partTwo = show . fromCoord . partTwoI
