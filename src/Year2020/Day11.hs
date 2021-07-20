module Year2020.Day11 where

import Prelude hiding (lookup) 
   
import Challenge
import Utils (countWhere)
  
import           Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as A

import Control.Monad (guard)
import Control.Applicative (empty)

type Position = (Int, Int)
type Grid = A.UArray Position Char

mkGrid :: String -> Grid
mkGrid ls = A.array ((0, 0), (rows, columns)) [((r, c), get r c) | r <- [0..rows], c <- [0..columns]]
  where parts = lines ls
        (rows, columns) = (length parts - 1, length (head parts) - 1)
        get = \row col -> (parts !! row) !! col

lookup :: Grid -> Position -> Maybe Char
grid `lookup` pos | A.inRange (A.bounds grid) pos = Just (grid ! pos)
                  | otherwise = Nothing 

type NeighborsFn = Position -> Grid -> Int

neighborsAdj :: NeighborsFn
neighborsAdj (row, col) grid = length $ do 
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  guard $ (dr, dc) /= (0, 0)
  let p = (row + dr, col + dc)
  guard $ A.inRange (A.bounds grid) p
  guard $ occupied (grid ! p)
  pure p

positions :: Position -> (Int, Int) -> [Position]
positions (r, c) (dr, dc) = [(r + dr * i, c + dc * i) | i <- [1..]]

firstOcc :: Grid -> [Position] -> [Char]
firstOcc grid = filter ('.' /=) . map (grid !) . takeWhile (A.inRange (A.bounds grid))

neighborsLOS :: NeighborsFn
neighborsLOS pos grid = length $ do
   dr <- [-1, 0, 1]
   dc <- [-1, 0, 1]
   guard $ (dr, dc) /= (0, 0)
   let p = firstOcc grid (positions pos (dr, dc))
   case p of
     ('#':_) -> pure p
     _       -> empty

type NextStateFn = Char -> Int -> Char

nextStateAdj :: NextStateFn
nextStateAdj 'L' n | n == 0 = '#'
nextStateAdj '#' n | n >= 4 = 'L'
nextStateAdj s   _          = s
  
nextStateLOS :: NextStateFn
nextStateLOS 'L' n | n == 0 = '#'
nextStateLOS '#' n | n >= 5 = 'L'
nextStateLOS s _            = s

step :: NeighborsFn -> NextStateFn -> Grid -> Grid
step nf ns grid = A.array (A.bounds grid) $ do
  (pos, state) <- A.assocs grid
  let nOccupiedNeighbors = nf pos grid
  pure (pos, ns state nOccupiedNeighbors) 

occupied :: Char -> Bool
occupied c = c == '#'

nOccupiedSeats :: Grid -> Int
nOccupiedSeats = countWhere occupied . A.elems

untilFix :: (Eq a) => (a -> a) -> a -> a
untilFix f x = if x == next then x else untilFix f next
  where next = f x

solve :: NeighborsFn -> NextStateFn -> Grid -> String
solve nf ns = show . nOccupiedSeats . untilFix (step nf ns)

instance Challenge Grid where
  parse = mkGrid
  partOne = solve neighborsAdj nextStateAdj
  partTwo = solve neighborsLOS nextStateLOS