{-# LANGUAGE TupleSections #-}
module Year2018.Day6 where
  
import Challenge
import Utils (readInt, manhattanDistance, frequencies)

import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Ix (range)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord    = (Int, Int)
type Voronoi  = M.Map Coord Coord

coord :: String -> (Int, Int)
coord s = case splitOn ", " s of
  [x, y] -> (readInt x, readInt y)
  _ -> error s
  
parseI :: String -> [Coord]
parseI = map coord . lines
                          
boundingBox :: [Coord] -> (Coord, Coord)
boundingBox cs = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where xs = map fst cs
        ys = map snd cs

closest :: Coord -> [Coord] -> Maybe Coord
closest c coords = if d1 == d2 then Nothing else Just cl   
  where ((cl, d1):(_, d2):_) = sortOn snd $ map (\r -> (r, manhattanDistance c r)) coords

isEdge :: (Coord, Coord) -> Coord -> Bool
isEdge ((minX, minY), (maxX, maxY)) (x, y) = or [minX == x, maxX == x, minY == y, maxY == y]
  
voronoi :: [Coord] -> [Coord] -> Voronoi
voronoi weights coords = M.fromList . catMaybes $ do
  c <- coords
  pure $ (c, ) <$> closest c weights
  
combinedManhattan :: [Coord] -> Coord -> Int
combinedManhattan coords c = sum $ manhattanDistance c <$> coords   
        
partOneI :: [Coord] -> Int
partOneI coords = maximum
                . frequencies
                . M.elems
                . M.filter (`S.notMember` edges)
                $ vs
  where bb    = boundingBox coords
        vs    = voronoi coords (range bb)
        edges = S.fromList [ site | (point, site) <- M.assocs vs, isEdge bb point]
        
partTwoI :: [Coord] -> Int
partTwoI coords = length
                . filter ((< 10000) . combinedManhattan coords)
                $ range bb
  where bb    = boundingBox coords

instance Challenge [Coord] where
  parse = parseI
  partOne = show . partOneI
  partTwo = show . partTwoI
  
  