module Year2019.Day1 where
import Challenge

fuel :: Int -> [Int]
fuel i
  | i <= 6 = []
  | otherwise = fu:fuel fu
    where fu = i `div` 3 - 2

instance Challenge [Int] where
  parse = map (\i -> read i :: Int) . lines
  partOne = show . sum . map (head . fuel)
  partTwo = show . sum . map (sum . fuel)

