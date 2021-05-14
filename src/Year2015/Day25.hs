module Year2015.Day25 where
import Challenge

fromCoords :: (Int, Int) -> Int
fromCoords (row, col) = r + col
  where
    sumUpTo n = (n * (n + 1)) `div` 2
    r = sumUpTo (row - 1 + col - 1)

codeAt :: (Int, Int) -> Int
codeAt coords = nextCode (fromCoords coords) 20151125
  where
    nextCode 1 c = c
    nextCode n c = nextCode (n - 1) (snd $ (c * 252533) `divMod` 33554393)

instance Challenge (Int, Int) where
  parse _ = (2978, 3083)
  partOne = show . codeAt
  partTwo _ = "Weather machine repaired!"
