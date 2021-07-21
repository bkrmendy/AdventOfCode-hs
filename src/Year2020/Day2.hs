module Year2020.Day2 where

import Challenge
import Utils (parseL, int, count, xor)

import Text.Parsec hiding (Line, count)

data Policy = Policy { _lo :: Int, _hi :: Int, _letter :: Char }

-- ^ 1-3 a: abcde
policy :: Parsec String () Policy
policy = Policy <$> (int <* char '-') <*> (int <* space) <*> (letter <* string ": ")

type Line = (Policy, String)

line :: Parsec String () Line
line = (,) <$> policy <*> many1 letter

type ValidF = Policy -> String -> Bool

valid1 :: ValidF
valid1 (Policy lo hi l) s = lo <= n && n <= hi
  where n = count l s

valid2 :: ValidF
valid2 (Policy a b l) s = (s !! (a - 1) == l) `xor` (s !! (b - 1) == l)

solve :: ValidF -> [Line] -> Int
solve f = length . filter (uncurry f)

instance Challenge [Line] where
  parse = parseL line
  partOne = show . solve valid1
  partTwo = show . solve valid2

