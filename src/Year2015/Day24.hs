{-# LANGUAGE FlexibleInstances #-}
module Year2015.Day24 where
import Challenge
import Data.List (subsequences, sortOn)

-- ^ https://stackoverflow.com/a/21288092
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

solve :: Integer -> Int -> [Integer] -> Integer
solve weight size = minimum . map product . sortOn length . filter ((==) weight . sum) . subsequencesOfSize size . reverse


instance Challenge [Integer] where
  parse = map (\a -> read a :: Integer) . lines
  partOne = show . solve 520 6
  partTwo = show . solve 390 4
