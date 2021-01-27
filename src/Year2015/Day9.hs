{-# LANGUAGE FlexibleInstances #-}
module Year2015.Day9 where
import Challenge
import Utils
import Data.List (permutations, nub)
import Text.Parsec

data Edge = Edge { from :: String, to :: String, distance :: Int }

parseDistance :: Parsec String () Edge
parseDistance = Edge <$> many1 letter <*> (string " to " *> many1 letter) <*> (string " = " *> int)

dup :: [Edge] -> [Edge]
dup [] = []
dup (Edge f t dst:rest) = [Edge t f dst, Edge f t dst] ++ dup rest

tsp :: [Edge] -> [Int]
tsp edges = map (sum . walk) (permutations cities)
  where
    cities = nub $ map from edges
    edge f t = head $ [e | e <- edges
                         , from e == f
                         , to e == t]
    walk [a, b] = [distance (edge a b)]
    walk (a:b:rest) = distance (edge a b):walk (b:rest)

instance Challenge [Edge] where
  parse = parseLines (sepBy1 parseDistance newline)
  partOne = show . minimum . tsp . dup
  partTwo = show . maximum . tsp . dup
