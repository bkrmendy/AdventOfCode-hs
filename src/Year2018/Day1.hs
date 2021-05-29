module Year2018.Day1 where
import Utils (parseL, int)
import Challenge
import Text.Parsec
import qualified Data.Set as S

type Change = Int

change :: Parsec String () Int
change = int

pt2 :: [Int] -> Int
pt2 = go S.empty 0 . cycle
  where
    go :: S.Set Int -> Int -> [Int] -> Int
    go seen x xs
      | S.member x seen = x
      | otherwise = go (S.insert x seen) (x + head xs) (tail xs)

instance Challenge [Change] where
  parse = parseL change
  partOne = show . sum
  partTwo = show . pt2  
