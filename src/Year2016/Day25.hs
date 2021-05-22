module Day25 where
import Data.List (find)
import Challenge
import Data.Maybe (fromJust)

-- ^ http://oeis.org/A000975
solve :: Int -> Maybe Int
solve target = (-) target <$> solution 
  where
    evenToOdd = zipWith (+) (cycle [0, 1])
    candidates = 1 : evenToOdd (map (*2) candidates)
    solution = find (> target) candidates

instance Challenge Int where
  parse _ = 4 * 633
  partOne = show . fromJust . solve
  partTwo _ = "Sleigh fixed!"