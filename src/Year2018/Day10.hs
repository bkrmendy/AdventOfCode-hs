{-# LANGUAGE FlexibleInstances #-}
module Year2018.Day10 where
import Text.Parsec
import Utils
import Challenge

import Debug.Trace

import qualified Data.Map as Map

data Position = Position { _col :: Int, _row :: Int} deriving (Eq, Ord)
data Velocity = Velocity { _dcol :: Int, _drow :: Int} deriving (Eq, Ord)
data Point = Point { _pos :: Position, _vel :: Velocity }

pPosition = Position <$> (string "position=<" *> int <* string ", ") <*> (int <* string ">")
pVelocity = Velocity <$> (string "velocity=<" *> int <* string ", ") <*> (int <* string ">")
pRecord = Point <$> (pPosition <* char ' ') <*> pVelocity

advance :: Position -> Velocity -> Position
advance (Position c r) (Velocity dc dr) = Position (c + dc) (r + dr)

step :: Int -> [Point] -> [[Point]]
step area field
  | nextArea > area = []
  | otherwise = field:step nextArea nextField -- ^ memory go brrr
    where
      nextField = [Point (advance pos vel) vel | (Point pos vel) <- field]
      (Position mic mir, Position mac mar) = aabb nextField
      nextArea = (mac - mic) * (mar - mir)

aabb :: [Point] -> (Position, Position)
aabb field = (Position minCol minRow, Position maxCol maxRow)
   where
       ps = map _pos field
       minRow = minimum (map _row ps)
       maxRow = maximum (map _row ps)
       minCol = minimum (map _col ps)
       maxCol = maximum (map _col ps)

display :: [Point] -> [String]
display field = [[get (Position col row) | col <- [minCol..maxCol]] | row <- [minRow..maxRow]]
  where
    points = map _pos field
    (Position minCol minRow, Position maxCol maxRow) = aabb field
    get pos = if pos `elem` points then '#' else '.'

largeInt :: Int
largeInt = 1000000000000000000

instance Challenge [Point] where
  parse = parseLines (sepBy1 pRecord newline)
  partOne = unlines . display . last . step largeInt
  partTwo = show . length . step largeInt


