{-# LANGUAGE FlexibleInstances #-}
module Year2019.Day3 where
import Challenge
import Utils (manhattan)
import Data.List (nubBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

data Instr = Instr Char Int

fromString :: String -> Instr
fromString (d:instr) = Instr d (read instr :: Int)

go :: Char -> (Int, Int) -> (Int, Int)
go 'U' (row, col) = (row - 1, col)
go 'D' (row, col) = (row + 1, col)
go 'L' (row, col) = (row, col - 1)
go 'R' (row, col) = (row, col + 1)

mark :: Int -> Int -> Map.Map (Int, Int) [(Int, Int)] -> (Int, Int) -> Map.Map (Int, Int) [(Int, Int)]
mark w s grid pos = newGrid
  where
    prevPos = Map.findWithDefault [] pos grid
    newGrid = Map.insert pos ((w, s):prevPos) grid

doInstr :: Int -> Int -> Map.Map (Int, Int) [(Int, Int)] -> (Int, Int) -> Instr -> ((Int, Int), Map.Map (Int, Int) [(Int, Int)])
doInstr w s grid pos (Instr dir times)
  | times == 1 = (nextPos, nextGrid)
  | otherwise = doInstr w (s + 1) nextGrid nextPos (Instr dir (times - 1))
    where
      nextPos = go dir pos
      nextGrid = mark w (s + 1) grid nextPos

draw :: Int -> (Int, Int) -> Int -> [Instr] -> Map.Map (Int, Int) [(Int, Int)] -> Map.Map (Int, Int) [(Int, Int)]
draw _ _   _     []           grid  = grid
draw w pos steps (instr:rest) grid  = draw w newPos (steps + l) rest newGrid
  where
    Instr _ l = instr
    (newPos, newGrid) = doInstr w steps grid pos instr

drawLines :: Map.Map (Int, Int) [(Int, Int)] -> [[Instr]] -> Map.Map (Int, Int) [(Int, Int)]
drawLines grid [f, s] = draw 2 (0, 0) 0 s (draw 1 (0, 0) 0 f grid)

instance Challenge [[Instr]] where
  parse = map (map fromString . splitOn ",") . splitOn "\n"
  partOne = show . minimum . map manhattan . Map.keys . Map.filter ((> 1) . length . nubBy (\a b -> fst a == fst b)) . drawLines Map.empty
  partTwo = show . minimum . Map.elems . Map.map (sum . map snd) . Map.filter ((> 1) . length . nubBy (\a b -> fst a == fst b)) . drawLines Map.empty
