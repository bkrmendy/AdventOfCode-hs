module Year2015.Day17 where
import Challenge
import Utils (subsets)
import Data.List (sort, group)

fittingContainers :: [Int] -> [[Int]]
fittingContainers = filter ((==) 150 . sum) . subsets

instance Challenge [Int] where
  parse = map (\i -> read i :: Int) . lines
  partOne = show . length . fittingContainers
  partTwo = show . length . head . group . sort . map length . fittingContainers
