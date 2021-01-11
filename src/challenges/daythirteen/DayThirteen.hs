{-# LANGUAGE TupleSections #-}

module DayThirteen where

import Challenge
import Data.Bits
import Utils
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence

newtype DesignersFavouriteNumber = DesignersFavouriteNumber { unDesignersFavouriteNumber :: Int }

isTile :: Int -> (Int, Int) -> Bool
isTile designer (x, y) = xPositive && yPositive && evenBits
  where
     xPositive  = x >= 0
     yPositive  = y >= 0
     evenBits   = even $ popCount $ x*x + 3*x + 2*x*y + y + y*y + designer

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

solve :: (Int, Int) -> Map.Map (Int, Int) Int -> Sequence.Seq (Int, Int) -> Int -> Maybe Int
solve target office frontier number =
  case Sequence.viewr frontier of
    Sequence.EmptyR -> Nothing
    (restOfFrontier Sequence.:> thisTile) ->
      if thisTile == target then Just $ unsafeGet thisTile office
      else solve' restOfFrontier thisTile
  where
    solve' restOfFrontier thisTile =
      let
        thisDistance = unsafeGet thisTile office
        neighborsOfTile = [tile | tile <- neighbors thisTile,
                                  isTile number tile,
                                  Map.notMember tile office]
        frontier' = Sequence.fromList neighborsOfTile Sequence.>< restOfFrontier
        office' = Map.union office (Map.fromList (map (, thisDistance + 1) neighborsOfTile))
      in solve target office' frontier' number

partOne' :: DesignersFavouriteNumber -> Int
partOne' = unsafeFromMaybe . solve target office frontier . unDesignersFavouriteNumber
  where
    target = (31, 39)
    office = Map.fromList [((1, 1), 0)]
    frontier = Sequence.fromList [(1, 1)]

partTwo' :: DesignersFavouriteNumber -> Int
partTwo' (DesignersFavouriteNumber n) = length reachables
  where
    steps tile = solve tile (Map.fromList [((1, 1), 0)]) (Sequence.fromList [(1, 1)]) n
    allTiles =  [(x, y) | x <- [0..50], y <- [0..50], isTile n (x, y)]
    distances = map steps allTiles
    reachables = [() | (Just distance) <- distances, distance <= 50]

instance Challenge DesignersFavouriteNumber where
   parse n = DesignersFavouriteNumber (read n :: Int)
   partOne = show . partOne'
   partTwo = show . partTwo'