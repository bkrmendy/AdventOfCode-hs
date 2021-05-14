module Year2017.Day15 where
import Challenge

import Data.Bits ((.&.))

parseI :: String -> (Int, Int)
parseI i = (getN a, getN b)
  where
    [a, b] = lines i
    getN = \s -> read (drop (length "Generator X starts with ") s) :: Int

type Generators = (Int, Int)

generator :: Int -> Int -> [Int]
generator seed factor = i:generator i factor
  where i = (seed * factor) `rem` 2147483647

compareBottomBits :: Int -> Int -> Int
compareBottomBits a b = if (a .&. 0xFFFF) == (b .&. 0xFFFF) then 1 else 0

mkGenerators :: (Int, Int) -> ([Int], [Int])
mkGenerators (a, b) = (generator a 16807, generator b 48271)

mkFancyGenerators :: (Int, Int) -> ([Int], [Int])
mkFancyGenerators (a, b) = (filter (divisible 4) $ generator a 16807, filter (divisible 8) $ generator b 48271)
  where
    divisible n = \i -> i `mod` n == 0

judge :: ([Int], [Int]) -> [Int]
judge (a, b) = zipWith compareBottomBits a b

solve :: Int -> ([Int], [Int]) -> Int
solve n = sum . take n . judge

instance Challenge Generators where
  parse = parseI
  partOne = show . solve 40000000 . mkGenerators
  partTwo = show . solve  5000000 . mkFancyGenerators
