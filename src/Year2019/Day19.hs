module Year2019.Day19 where

import Challenge
import qualified Intcode as IC

import Control.Monad (guard)

tractor :: IC.Program -> (Int, Int) -> Bool
tractor program (x, y)
  = case IC.executeCode [x, y] program of
    [r] -> r > 0
    o   -> error $ "Invalid output: " ++ show o

picture ::  Int -> Int -> IC.Program -> Int
picture maxX maxY program = length $ do
   x <- [0..maxX]
   y <- [0..maxY]
   guard $ tractor program (x, y)
   pure (x, y)

rightEdgeOfBeam :: Int -> IC.Program -> Int
rightEdgeOfBeam row program = head $ do
  col <- [0..]
  guard $ tractor program (row, col) && not (tractor program (row, col + 1))
  pure col

fits :: IC.Program -> Int -> (Int, Int) -> Bool
fits program santaSize (row, col) = topLeft && bottomLeft
  where
    startCol = col - santaSize + 1
    topLeft = tractor program (row, startCol)
    bottomLeft = tractor program (row + santaSize - 1, startCol)

pt2 :: Int -> Int -> IC.Program -> (Int, Int)
pt2 startRow santaSize program = head $ do
  row <- [startRow..]
  let reb = rightEdgeOfBeam row program
  guard $ fits program santaSize (row, reb)
  pure (row, reb - santaSize + 1)

pt2Result :: (Int, Int) -> Int
pt2Result (x, y) = x * 10000 + y

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . picture 49 49
  partTwo = show . pt2Result . pt2 1865 100 -- seed


