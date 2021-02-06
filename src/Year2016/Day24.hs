{-# LANGUAGE FlexibleInstances #-}

module Day24 where
import Challenge
import Utils
import Data.Char (isDigit, digitToInt)
import Data.List (nub, delete, minimumBy, permutations)
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Heap as Heap

data Tile = Wall
          | Space
          | PoI Int

fromChar :: Char -> Tile
fromChar '#' = Wall
fromChar '.' = Space
fromChar c
  | isDigit c = PoI (digitToInt c)
  | otherwise = error ("Unrecognized tile marker: " ++ show c)

isWall :: Tile -> Bool
isWall Wall = True
isWall _    = False

type Coord = (Int, Int)
type Grid  = Map.Map Coord Tile

data Edge = Edge { from :: Int, to :: Int, distance :: Int} deriving (Show)

type Queue = Seq.Seq (Coord, Int)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

bfs :: Grid -> Queue -> Set.Set Coord -> [(Int, Int)]
bfs grid queue seen
  | Seq.null queue = []
  | otherwise = pois thisTile ++ rest
  where
    (thisTile, thisDistance) = Seq.index queue 0
    ok coord = Map.member coord grid && not (Set.member coord seen) && not (isWall (grid Map.! coord))
    nextNeighbors = filter ok (neighbors thisTile)
    nextSeen = inserts nextNeighbors seen
    nextQueue = Seq.drop 1 queue Seq.>< Seq.fromList (map (\n -> (n, thisDistance + 1)) nextNeighbors)
    pois tile = case grid Map.! tile of
      PoI n -> [(n, thisDistance)]
      _ -> []
    rest = bfs grid nextQueue nextSeen

tsp :: [Edge] -> (Int, Int)
tsp edges = (path, cycle)
  where
    path = minimum $ map (\s -> edge 0 (head s) + walk s) (permutations [0..7])
    cycle = minimum $ map (\s -> edge 0 (head s) + walk s + edge (last s) 0) (permutations [0..7])
    edge src dst = head [d | Edge f t d <- edges
                           , f == src
                           , t == dst]
    walk :: [Int] -> Int
    walk [a, b] = edge a b
    walk (a:b:rest) = edge a b + walk (b:rest)
    walk _ = error "Too few vertices"

findDistances :: Grid -> [Edge]
findDistances grid = concatMap findDistance pois
  where
    pois = [(crd, n) | (crd, PoI n) <- Map.assocs grid]
    distances c = bfs grid (Seq.singleton (c, 0)) (Set.singleton c)
    findDistance (c, n) = map (uncurry (Edge n)) (distances c)

fromLines :: [String] -> Grid
fromLines grid = Map.fromList $ concatMap makeRow (zip grid [0..])
  where
    makeRow :: (String, Int) -> [((Int, Int), Tile)]
    makeRow (row, rowN) = [((rowN, colN), fromChar chr) | (chr, colN) <- zip row [0..]]

instance Challenge [String] where
  parse = lines
  partOne = show . fst . tsp . findDistances . fromLines
  partTwo = show . snd . tsp . findDistances . fromLines