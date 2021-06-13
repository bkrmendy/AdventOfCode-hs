{-# LANGUAGE BangPatterns #-}
module Year2018.Day9 where

import Challenge
import Utils (readInt)

import Data.List (foldl')
import qualified Data.Vector.Unboxed as V

parseI :: String -> Game
parseI i = case words i of
  (m:"players;":"last":"marble":"is":"worth":n:"points":_) -> Game (readInt m) (readInt n)
  _ -> error i

data Game = Game { _nPlayers :: Int, _maxMarble :: Int }
data PointedList a = PointedList { _reversedPrefix :: [a], _focus :: a, _suffix :: [a] }

singleton :: a -> PointedList a
singleton x = PointedList [] x []

-- | Inserts `a` clockwise of the second param
insert :: a -> PointedList a -> PointedList a
insert x (PointedList ccw i cw) = PointedList (i:ccw) x cw

-- | Removes the focus of the list, returning the clockwise element
remove :: PointedList a -> PointedList a
remove (PointedList _   _ [])      = error "Cannot remove clockwise element"
remove (PointedList ccw _ (c:cw)) = PointedList ccw c cw

next :: PointedList a -> PointedList a
next p@(PointedList [] _ []) = p
next   (PointedList ccw i []) = let (x:xs) = reverse ccw in  PointedList [] x (xs ++ [i])
next   (PointedList ccw i (c:cw)) = PointedList (i:ccw) c cw

prev :: PointedList a -> PointedList a
prev p@(PointedList [] _ []) = p
prev   (PointedList [] i cw) = let (x:xs) = reverse cw in PointedList (xs ++ [i]) x []
prev   (PointedList (c:ccw) i cw) = PointedList ccw c (i:cw)

clockwise, counterClockwise :: Int -> PointedList a -> PointedList a
clockwise 0 = id
clockwise n = clockwise (n - 1) . next
counterClockwise 0 = id
counterClockwise n = counterClockwise (n - 1) . prev

step :: Int -> PointedList Int -> (Int, PointedList Int)
step n p
  | n `rem` 23 == 0 = (n + _focus sevenDown, remove sevenDown)
  | otherwise = (0, (insert n . clockwise 1) p)
  where
    sevenDown = counterClockwise 7 p

run :: Int -> Int -> Int
run nPlayers maxMarble = V.maximum . fst . foldl' go (V.replicate nPlayers 0, singleton 0) $ zip players marbles
  where
    players = [x `rem` nPlayers | x <- [0..]]
    marbles = [1..maxMarble]
    go :: (V.Vector Int, PointedList Int) -> (Int, Int) -> (V.Vector Int, PointedList Int)
    go (!scores, !ms) (!p, !m) = (nextScores, nextMarbles)
      where
        (score, nextMarbles) = step m ms
        nextScores = scores V.// [(p, (scores V.! p) + score)]

kickoff :: Int -> Game -> Int
kickoff multiplier (Game players maxMarble) = run players (maxMarble * multiplier)

instance Challenge Game where
  parse = parseI
  partOne = show . kickoff 1
  partTwo = show . kickoff 100

