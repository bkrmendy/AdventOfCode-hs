{-# LANGUAGE TupleSections #-}
module Year2017.Day21 where
import            Challenge
import            Utils (parseL, countWhere)

import            Data.List (transpose)
import            Text.Parsec hiding (State)
import qualified  Data.Map.Strict  as M
import qualified  Data.Array       as A

type Pattern = ([String], String)

patternChar :: Parsec String () Char
patternChar = (char '.') <|> (char '#')

sepBySlash :: Parsec String () [String]
sepBySlash = sepBy1 (many1 patternChar) (char '/')

pPattern :: Parsec String () Pattern
pPattern = (,) <$> sepBySlash <*> (string " => " *> (concat <$> sepBySlash))

type Grid = A.Array (Int, Int) Char
type Book = M.Map String String

rotate :: [[a]] -> [[a]]
rotate = transpose . map reverse

rotations :: [String] -> [String]
rotations pat =
  let
    r1 = rotate pat
    r2 = rotate r1
    r3 = rotate r2
  in map concat [pat, r1, r2, r3]

enhance :: Pattern -> [(String, String)]
enhance (pattern, match) = go pattern ++ go (map reverse pattern)
  where go = \pat -> map ((,match)) (rotations pat)

matchFromBook :: Book -> String -> String
matchFromBook book pattern = book M.! pattern

lookupFromGrid :: Grid -> [(Int, Int)] -> String
lookupFromGrid grid = map (grid A.!)

getNextSquare :: Book -> Grid -> [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), Char)]
getNextSquare book grid fromCoords toCoords = zip toCoords (get fromCoords)
  where get = matchFromBook book . lookupFromGrid grid

squares :: Int -- ^ side length of generated square
        -> Int -- ^ number of generated squares on side
        -> [[(Int, Int)]]
squares side n = [go nc nr | nc <- [0..n - 1], nr <- [0..n - 1]]
  where
    go = \nc nr -> [(c, r) | c <- [nc*side..nc*side + side - 1], r <- [nr*side..nr*side + side - 1]]

stepI :: Book -> Grid -> Int -> Int -> Grid
stepI book grid dFrom dTo = nextGrid
  where
    (_, (width, _)) = A.bounds grid
    sideSquares = (width + 1) `div` dFrom   -- ^ number of squares on one side in the from grid
    squaresFrom = squares dFrom sideSquares -- ^ squares generated from the the from gird
    squaresTo = squares dTo sideSquares     -- ^ squares generated from the the to gird
    nextDim = sideSquares * dTo - 1         -- ^ next max dimension
    nextSquares = concatMap (uncurry (getNextSquare book grid)) (zip squaresFrom squaresTo)
    nextGrid = A.array ((0, 0), (nextDim, nextDim)) nextSquares

step :: Book -> Grid -> Grid
step book grid = nextGrid
  where
    (_, (width, _)) = A.bounds grid
    nextGrid = if (even $ width + 1) then stepI book grid 2 3 else stepI book grid 3 4

nPixelsOn :: Grid -> Int
nPixelsOn = countWhere (== '#') . A.elems
  
mkGrid :: Grid
mkGrid = A.array ((0, 0), (2, 2)) [((c, r), get c r) | c <- [0, 1, 2], r <- [0, 1, 2]]
  where
    get = \c r -> (pattern !! r) !! c
    pattern = [ ".#."
              , "..#"
              , "###"
              ]     

solve :: Int -> Book -> Int
solve iterations book = nPixelsOn . last . take (iterations + 1) $ iterate (step book) mkGrid

instance Challenge Book where
  parse = M.fromList . concatMap enhance . parseL pPattern
  partOne = show . solve 5
  partTwo = show . solve 18


