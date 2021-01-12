module DayNineteen where
import Challenge
import qualified Data.Sequence as Seq

newtype NElves = NElves { unNElves :: Float }

-- ^ https://en.wikipedia.org/wiki/Josephus_problem
partOneI :: Float -> Float
partOneI n = 1 + 2 * (n - 2 ^ floor (logBase 2 n))

-- ^ adapted from https://github.com/glguy/advent2016/blob/master/Day19.hs
partTwoI :: Seq.Seq Int -> Maybe Int
partTwoI xs =
  case Seq.viewl xs of
    Seq.EmptyL -> Nothing
    x Seq.:< ys
      | Seq.null ys -> Just x
      | otherwise -> partTwoI ((l Seq.>< Seq.drop 1 r) Seq.|> x)
          where (l, r) = Seq.splitAt (half (length ys)) ys
                half len = (len-1) `div` 2

instance Challenge NElves where
    parse i = NElves (read i :: Float)
    partOne = show . partOneI . unNElves
    partTwo = show . partTwoI . Seq.fromList . enumFromTo 1 . round . unNElves
