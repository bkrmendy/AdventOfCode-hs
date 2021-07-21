module Year2020.Day12 where
  
import Challenge
import Utils (readInt)

import Data.List (foldl')
import Data.Maybe (mapMaybe)

import Debug.Trace

-- ^ horizontal, vertical
type Heading = (Int, Int)
type Position = (Int, Int)
type Waypoint = (Int, Int)

data Action
  = N Int
  | S Int
  | E Int
  | W Int
  | L Int
  | R Int
  | F Int
  
action :: String -> Maybe Action
action [] = Nothing
action s = case head s of
  'N' -> Just (N steps)
  'S' -> Just (S steps)
  'E' -> Just (E steps)
  'W' -> Just (W steps)
  'L' -> Just (L steps)
  'R' -> Just (R steps)
  'F' -> Just (F steps)
  _   -> Nothing
  where steps = readInt (tail s)
  
rotate :: Heading -> Int -> Heading
rotate h 0 = h
rotate (dh, dv) d | d < 0 = rotate (dv, - dh) (d + 90)
                  | d > 0 = rotate (- dv, dh) (d - 90)
                  
rotateAround :: Position -> Heading -> Int -> Heading
rotateAround (ph, pv) (wh, wv) d = (rh + ph, rv + pv)
  where (rh, rv) = rotate (wh - ph, wv - pv) d

type StepFn = (Position, (Int, Int)) -> Action -> (Position, (Int, Int))  

stepPt1 :: (Position, Heading) -> Action -> (Position, Heading)
stepPt1 ((ph, pv), heading) (N i) = ((ph, pv - i), heading)
stepPt1 ((ph, pv), heading) (S i) = ((ph, pv + i), heading)
stepPt1 ((ph, pv), heading) (E i) = ((ph + i, pv), heading)
stepPt1 ((ph, pv), heading) (W i) = ((ph - i, pv), heading)
stepPt1 (pos, heading)      (L i) = (pos, rotate heading (- i))
stepPt1 (pos, heading)      (R i) = (pos, rotate heading i)
stepPt1 ((ph, pv), (dh, dv)) (F i) = ((ph + dh * i, pv + dv * i), (dh, dv))

stepPt2 :: (Position, Waypoint) -> Action -> (Position, Waypoint)
stepPt2 (pos, (wh, wv)) (N i) = (pos, (wh, wv - i))
stepPt2 (pos, (wh, wv)) (S i) = (pos, (wh, wv + i))
stepPt2 (pos, (wh, wv)) (W i) = (pos, (wh - i, wv)) 
stepPt2 (pos, (wh, wv)) (E i) = (pos, (wh + i, wv))
stepPt2 (pos, waypoint) (L i) = (pos, rotate waypoint (- i))
stepPt2 (pos, waypoint) (R i) = (pos, rotate waypoint i)
stepPt2 ((ph, pv), (wh, wv)) (F i) = ((ph + wh * i, pv + wv * i), (wh, wv))

distance :: (Position, Heading) -> Int
distance ((h, v), _) = abs h + abs v

instance Challenge [Action] where
  parse = mapMaybe action . lines
  partOne = show . distance . foldl' stepPt1 ((0, 0), (1, 0))
  partTwo = show . distance . foldl' stepPt2 ((0, 0), (10, -1))