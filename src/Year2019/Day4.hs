module Year2019.Day4 where
import Challenge

same is     = or (zipWith (==) is (tail is))
increase is = and (zipWith (<=) is (tail is))
count l is  = length (filter (l ==) is)
double is   = or (zipWith (\a b -> a == b && count a is == 2) is (tail is))

solve :: (String -> Bool) -> (Int, Int) -> [Int]
solve ok (lo, hi) = [pw | pw <- [lo..hi], ok (show pw)]

check :: [String -> Bool] -> String -> Bool
check preds i = all (\p -> p i) preds

instance Challenge (Int, Int) where
  parse _ = (134564, 585159)
  partOne = show . length . solve (check [same, increase])
  partTwo = show . length . solve (check [double, increase])
