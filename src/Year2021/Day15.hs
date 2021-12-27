module Year2021.Day15 where
  
import Challenge
import Utils (spfa)

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe, fromJust)  
import qualified Data.Map.Strict as M

type Map = M.Map (Int, Int) Int

parseI :: String -> Map
parseI input = M.fromList $ do
  (row, iRow) <- zip (lines input) [1..]
  (col, iCol) <- zip row [1..]
  pure ((iRow, iCol), digitToInt col)
  
neighbors :: Map -> (Int, Int) -> [(Int, (Int, Int))]
neighbors m (row, col) = mapMaybe find [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
  where
    find coord = do
      value <- coord `M.lookup` m
      pure (value, coord)
  
findPath :: Map -> Int
findPath m = fromJust $ do
  ((end, _), _) <- M.maxViewWithKey m
  result <- spfa step end (0, (1, 1))
  pure $ fst result
  where step (cost, node) = [(cost + c, n) | (c, n) <- neighbors m node]

mod9 :: Int -> Int
mod9 x = (x - 1) `mod` 9 + 1 
  
boost :: Map -> Map
boost m = M.fromList [ (kk, mod9 $ v + dr + dc)
                     | ((r, c), v) <- M.toList m
                      , dr <- [0..4]
                      , dc <- [0..4]
                      , let kk = (mh * dr + r, mw * dc + c)]
  where (mw, mh) = fst $ M.findMax m
      
instance Challenge Map where
  parse = parseI
  partOne = show . findPath
  partTwo = show . findPath . boost
  

