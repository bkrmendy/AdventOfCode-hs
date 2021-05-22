module Year2017.Day17 where
import            Challenge
import            Utils (readInt)
import            Data.Maybe (fromJust)
import qualified  Data.Sequence as Seq

step :: Int -> (Int, Seq.Seq Int) -> Int -> (Int, Seq.Seq Int)
step stepSize (pos, is) value = (nextPos, Seq.insertAt nextPos value is)
  where
    nextPos = (pos + stepSize) `mod` Seq.length is + 1

after :: Int -> Seq.Seq Int -> Int
after ix es = Seq.index es $ (ix + 1) `mod` Seq.length es

partOneI :: Int -> Int
partOneI stepSize = after i es
  where
    (i, es) = foldl (step stepSize) (0, Seq.singleton 0) [1..2017]

partTwoI :: Int -> Int
partTwoI stepSize = after (fromJust $ Seq.elemIndexL 0 es) es
  where
      (_, es) = foldl (step stepSize) (0, Seq.singleton 0) [1..50000000]

instance Challenge Int where
  parse = readInt
  partOne = show . partOneI
  partTwo = show . partTwoI
