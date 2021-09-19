module Year2020.Day24 where

import Challenge
import Utils (toggle, composeN)
import GameOfLife

import Data.Function ((&))
import Data.List (foldl')  
import qualified Data.Set as S

data Coord = Coord { _row :: Int, _column :: Int } deriving (Eq, Ord)
type Direction = Coord -> Coord

e, w, ne, nw, se, sw :: Coord -> Coord
e (Coord r c) = Coord r (c + 1)
w (Coord r c) = Coord r (c - 1)
nw (Coord r c) | even r = Coord (r - 1) (c - 1)
nw (Coord r c)          = Coord (r - 1) c
se (Coord r c) | even r = Coord (r + 1) c
se (Coord r c)          = Coord (r + 1) (c + 1)
ne (Coord r c) | even r = Coord (r - 1) c
ne (Coord r c)          = Coord (r - 1) (c + 1)
sw (Coord r c) | even r = Coord (r + 1) (c - 1)
sw (Coord r c)          = Coord (r + 1) c

type Path = [Direction]

path :: String -> Path
path [] = []
path ('s':'e':rest) = se:path rest
path ('s':'w':rest) = sw:path rest
path ('n':'e':rest) = ne:path rest
path ('n':'w':rest) = nw:path rest
path ('e':rest)     = e:path rest
path ('w':rest)     = w:path rest

walk :: Path -> Coord
walk = foldl' (&) (Coord 0 0)

partOneI :: [Path] -> Int
partOneI paths = S.size tiles
  where tiles = foldr (toggle . walk) S.empty paths 

instance GameOfLife Coord where
  neighbors c = [e c, w c, nw c, sw c, se c, ne c]
  transition n (Active c)   | n == 1 || n == 2 = Just c
  transition n (Inactive c) | n == 2         = Just c
  transition _ _                             = Nothing
  
  
partTwoI :: [Path] -> Int
partTwoI paths = S.size (time tiles)
  where tiles = foldr (toggle . walk) S.empty paths
        time = composeN step 100
  
instance Challenge [Path] where
  parse = map path . lines
  partOne = show . partOneI
  partTwo = show . partTwoI