module Year2015.Day5 where
import Challenge
import Data.List (isInfixOf, group, sort)

partOneFilter :: String -> Bool
partOneFilter str = hasThreeVowels && hasTwiceInRow && noForbiddenStrings
  where
    hasThreeVowels = (> 2) $ length $ filter (`elem` "aeiou") str
    hasTwiceInRow = any ((>1) . length) $ group str
    noForbiddenStrings = not $ any (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [a,b] = [(a, b)]
pairs (a:b:rest) = (a, b):pairs (b:rest)

threes :: [a] -> [(a, a, a)]
threes [] = []
threes [a, b, c] = [(a, b, c)]
threes (a:b:c:rest) = (a, b, c):threes (b:c:rest)

partTwoFilter :: String -> Bool
partTwoFilter str = anyTwo str && repeats && noThrees
  where
    anyTwo = any ((>1) . length) . group . sort . pairs
    repeats = any (\(a, _, c) -> a == c) $ threes str
    noThrees = not $ any (\(a, b, c) -> a == c && a == b) $ threes str

instance Challenge [String] where
  parse = lines
  partOne = show . length . filter partOneFilter
  partTwo = show . length . filter partTwoFilter