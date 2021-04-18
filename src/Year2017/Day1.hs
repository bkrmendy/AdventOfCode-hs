module Year2017.Day1 where
import Challenge
import Data.List (group)
import Data.Char (digitToInt)

type Lookup = Int -> Int

solve :: Lookup -> [Int] -> Int
solve f ds = foldr captcha 0 [0..length ds - 1]
  where
    sStr = cycle ds
    captcha i acc = if sStr !! (f i) == sStr !! i then acc + (sStr !! i) else acc



instance Challenge [Int] where
  parse = map digitToInt
  partOne = show . solve (+ 1)
  partTwo ds = show (solve (+ (length ds `div` 2)) ds)