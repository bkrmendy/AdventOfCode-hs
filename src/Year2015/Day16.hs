module Year2015.Day16 where
import Challenge
import Utils (parseLines, int)
import Data.List (sortOn)
import qualified Data.Map as Map

import Text.Parsec

data Sue = Sue { _no :: Int, _attrs :: [(String, Int)] }

attr :: Parsec String () (String, Int)
attr = (,) <$> many1 letter <*> (string ": " *> int)

sue :: Parsec String () Sue
sue = Sue <$> (string "Sue " *> int) <*> (string ": " *> sepBy1 attr (string ", "))

-- from problem description
attrs :: Map.Map String Int
attrs = Map.fromList [
    ("children",    3)
  , ("cats",        7)
  , ("samoyeds",    2)
  , ("pomeranians", 3)
  , ("akitas",      0)
  , ("vizslas",     0)
  , ("goldfish",    5)
  , ("trees",       3)
  , ("cars",        2)
  , ("perfumes",    1)
  ]

type Matcher = String -> Int -> Bool 

matches :: Matcher
matches a given = given == attrs Map.! a

matchesPt2 :: Matcher
matchesPt2 "cats" given = given > attrs Map.! "cats"
matchesPt2 "trees" given = given > attrs Map.! "trees"
matchesPt2 "pomeranians" given = given < attrs Map.! "pomeranians"
matchesPt2 "goldfish" given = given < attrs Map.! "goldfish"
matchesPt2 a given = given == attrs Map.! a

score :: Matcher -> Sue -> Int
score m (Sue _ attributes) = length [() | (a, v) <- attributes, m a v]

rank :: Matcher -> [Sue] -> [Sue]
rank m = reverse . sortOn (score m)

instance Challenge [Sue] where
  parse = parseLines (sepBy1 sue newline)
  partOne = show . _no . head . rank matches
  partTwo = show . _no . head . rank matchesPt2
  



