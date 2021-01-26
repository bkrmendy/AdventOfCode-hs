module Day25 where
import Data.Maybe (catMaybes)
import Data.List (find)
import Challenge

-- ^ http://oeis.org/A000975
solve :: Int -> Int
solve target = solution - target
  where
    evenToOdd = zipWith (+) (cycle [0, 1])
    candidates = 1 : evenToOdd (map (*2) candidates)
    Just solution = find (>target) candidates

instance Challenge Int where
  parse _ = 4 * 633
  partOne = show . solve
  partTwo _ = "Sleigh fixed!"