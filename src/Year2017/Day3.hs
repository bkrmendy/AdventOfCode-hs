module Year2017.Day3 where
import Challenge
import Utils (readInt, manhattan)
import qualified Data.Array as A

{-
37  36  35  34  33  32  31
38  17  16  15  14  13  30
39  18   5   4   3  12  29
40  19   6   1   2  11  28
41  20   7   8   9  10  27
42  21  22  23  24  25  26
43  44  45  46  47  48  49  50
                            81
-}

layerWidth :: Int -> Int
layerWidth i = 2 * i - 1

up :: Int -> [(Int, Int)]
up layer =
  let
    width = layerWidth layer
    (startingColumn, startingRow) = (layer - 1, - (layer - 1))
  in [(startingColumn, startingRow + d) | d <- [1..width - 1]]

left :: Int -> [(Int, Int)]
left layer =
  let
    width = layerWidth layer
    (startingColumn, startingRow) = (layer - 1, layer - 1)
  in [(startingColumn - d, startingRow) | d <- [1..width - 1]]

down :: Int -> [(Int, Int)]
down layer =
  let
    width = layerWidth layer
    (startingColumn, startingRow) = (-(layer - 1), layer - 1)
  in [(startingColumn, startingRow - d) | d <- [1..width - 1]]

right :: Int -> [(Int, Int)]
right layer =
  let
    width = layerWidth layer
    (startingColumn, startingRow) = (-(layer - 1), -(layer - 1))
  in [(startingColumn + d, startingRow) | d <- [1..width - 1]]

coords :: Int -> [(Int, Int)]
coords 1     = [(0, 0)]
coords layer = up layer <> left layer <> down layer <> right layer

partOneI :: Int -> Int
partOneI n = manhattan coord
  where
    ((coord, _):_) = filter ((== n) . snd) $ zip (concatMap coords [1..]) [1..]


-- | TODO: part 2, I got rekt by this
instance Challenge Int where
  parse = readInt
  partOne = show . partOneI