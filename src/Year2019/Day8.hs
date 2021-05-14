module Year2019.Day8 where
import Challenge
import Utils (count)
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import Data.Char (digitToInt)

decode :: [[Int]] -> [Int]
decode = foldl1 stack
  where
    stack = zipWith combine
    combine 2 n = n
    combine n 2 = n
    combine n _ = n

prettify :: [Int] -> String
prettify = map disp
  where
    disp 0 = ' '
    disp 1 = '#'

instance Challenge [[Int]] where
  parse = map (map digitToInt) . chunksOf (25 * 6)
  partOne = show . (\xs -> count 1 xs * count 2 xs) . head . sortOn (count 0)
  partTwo = unlines . map prettify . chunksOf 25 . decode
