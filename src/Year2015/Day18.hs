module Year2015.Day18 where
import Challenge

import Prelude hiding (lookup)
import Data.Maybe (catMaybes)
import qualified Data.Array as Array

data Light = On | Off deriving (Eq)

type Garden = Array.Array (Int, Int) Light

lightsOn :: Garden -> Int
lightsOn = length . filter (== On) . Array.elems

fromAscii :: Char -> Light
fromAscii '#' = On
fromAscii '.' = Off

toggle :: [Light] -> Light -> Light
toggle ns light =
  let neighborsOn = length $ filter (== On) ns in
  case light of
      On -> if neighborsOn == 2 || neighborsOn == 3 then On else Off
      Off -> if neighborsOn == 3 then On else Off

lookup :: (Int, Int) -> Garden -> Maybe Light
lookup ix garden
  | Array.inRange (Array.bounds garden) ix = Just (garden Array.! ix)
  | otherwise = Nothing

neighbors :: (Int, Int) -> Garden -> [Light]
neighbors (r, c) garden = catMaybes [lookup (ri, ci) garden | ri <- [r-1..r+1], ci <- [c-1..c+1], (ri, ci) /= (r, c)]

type Next = (Int, Int) -> Garden -> Light

nextPt1 :: Next
nextPt1 ix garden = toggle (neighbors ix garden) (garden Array.! ix)

nextPt2 :: Next
nextPt2 (0,  0 ) = const On
nextPt2 (0,  99) = const On
nextPt2 (99, 0 ) = const On
nextPt2 (99, 99) = const On
nextPt2 ix       = nextPt1 ix

step :: Next -> Garden -> Garden
step next garden = Array.array ((0, 0), (99, 99)) [ ((r, c), next (r, c) garden) | r <- [0..99], c <- [0..99]]

gardenFromStrings :: [String] -> Garden
gardenFromStrings strs = Array.array ((0, 0), (99, 99)) [ ((r, c), fromAscii $ strs !! r !! c) | r <- [0..99], c <- [0..99]]

solve :: Next -> Garden -> Int
solve next = lightsOn . last . take 101 . iterate (step next)


instance Challenge Garden where
  parse = gardenFromStrings . lines
  partOne = show . solve nextPt1
  partTwo = show . solve nextPt2
