module Year2017.Day2 where
import Challenge
import Utils (parseL, int)

import Text.Parsec

type Checksum = [Int] -> Int

parseRow :: Parsec String () [Int]
parseRow = sepBy1 int tab

minMax :: [Int] -> Int
minMax is = maximum is - minimum is

evenlyDivisible :: [Int] -> Int
evenlyDivisible is = r
  where (r:_) = [ d | a <- is
                    , b <- is
                    , a /= b
                    , let (d, m) = a `divMod` b
                    , m == 0
                    ]

check :: Checksum -> [[Int]] -> Int
check checksum = sum . map checksum

instance Challenge [[Int]] where
  parse = parseL parseRow
  partOne = show . check minMax
  partTwo = show . check evenlyDivisible

