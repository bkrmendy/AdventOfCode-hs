module Year2017.Day13 where
import            Challenge
import            Utils (parseL, int)

import            Data.Maybe (fromJust)
import            Data.List (find)
import            Text.Parsec hiding (State)

type Firewall = [(Int, Int)]

pLayer :: Parsec String () (Int, Int)
pLayer = (,) <$> int <*> (string ": " *> int)

severity :: Int -> [(Int, Int)] -> Int
severity delay fw = sum [l * p | (l, p) <- fw, (l + delay) `mod` (2 * p - 2) == 0]

instance Challenge Firewall where
  parse = parseL pLayer
  partOne = show . severity 0
  partTwo fw = show . fromJust $ find (\i -> severity i fw == 0) [0..] -- TODO: not good



