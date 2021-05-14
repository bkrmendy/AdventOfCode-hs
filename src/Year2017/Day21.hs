{-# LANGUAGE TupleSections #-}
module Year2017.Day21 where
import            Challenge
import            Utils (parseL, countWhere)

import            Data.List (transpose)
import            Text.Parsec hiding (State)
import qualified  Data.Map.Strict  as M
import qualified  Data.Array       as A

type Pattern = ([String], [String])

pPattern :: Parsec String () Pattern
pPattern = (,) <$> (sepBy1 (many1 anyChar) (char '/')) <*> (string " => " *> sepBy1 (many1 anyChar) (char '/'))

type Grid = A.Array (Int, Int) Char
type Book = M.Map [String] [String]

rotate :: [[a]] -> [[a]]
rotate = transpose . map reverse

enhance :: Pattern -> [Pattern]
enhance (pattern, match) = go pattern ++ go (map reverse pattern)
  where go = \pat -> let
                        r1 = rotate pat
                        r2 = rotate r1
                        r3 = rotate r2
                      in map ((,match)) [pat, r1, r2, r3]



step2 :: Book -> Grid -> Grid
step2 = undefined

step3 :: Book -> Grid -> Grid
step3 = undefined

squares :: Int -- ^ side length of generated square (2 or 3)
        -> Int -- ^ number of generated squares
        -> [[(Int, Int)]]
squares side n = [go nc nr | nc <- [0..n - 1], nr <- [0..n - 1]]
  where
    go = \nc nr -> [(c, r) | c <- [nc*side..nc*side + side - 1], r <- [nr*side..nr*side + side - 1]]

step :: Book -> Grid -> Grid
step book grid = if even (width + 1) then step2 book grid else step3 book grid
  where (_, (width, _)) = A.bounds grid
  
nPixelsOn :: Grid -> Int
nPixelsOn = countWhere (== '#') . A.elems
  
mkGrid :: Grid
mkGrid = A.array ((0, 0), (2, 2)) [((c, r), (pattern !! r) !! c) | c <- [0, 1, 2], r <- [0, 1, 2]]
  where
    pattern = [ ".#."
              , "..#"
              , "###"
              ]     

instance Challenge Book where
  parse = M.fromList . concatMap enhance . parseL pPattern
  partOne book = show . nPixelsOn . last . take 5 $ iterate (step book) mkGrid


