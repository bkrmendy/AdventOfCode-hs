module Year2015.Day15 where
import Data.List (nub, sort)
import Utils
import Challenge

ingredients :: [[Int]]
ingredients = [ [4, -2, 0, 0, 5]
              , [0, 5, -1, 0, 8]
              , [-1, 0, 5, 0, 6]
              , [0, 0, -2, 2, 1]
              ]

ingredientsEx :: [[Int]]
ingredientsEx = [ [-1, -2, 6, 3, 8]
                , [2, 3, -2, -1, 3]
                ]

partitions :: Int -> [[Int]]
partitions n = [[i, j, k, l] | i <- [0..n]
                             , j <- [0..n-i]
                             , k <- [0..n-i-j]
                             , l <- [0..n-i-j-k]]

total :: [Int] -> [[Int]] -> Int
total spoons ingrs = product $ map (combine spoons) (init ingrs)
  where
    combine :: [Int] -> [Int] -> Int
    combine sp i = max 0 $ sum $ zipWith (*) sp i

recipe :: ([Int] -> [Int] -> Bool) -> [[Int]] -> Int
recipe cals is = maximum $ [total part is | part <- partitions 100
                                            , cals part (last is)]

instance Challenge [[Int]] where
  parse _ = ingredients
  partOne = show . recipe (\_ _ -> True). transpose
  partTwo = show . recipe (\sps cals -> 500 == sum (zipWith (*) sps cals)) . transpose
