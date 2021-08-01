module Year2020.Day13 where

import Challenge  
import Utils (readInt, chineseRemainder)

import Data.List.Split (splitOn)

data Bus = Bus { _id :: Int, _offset :: Int }
data Schedule = Schedule { _depart :: Int, _buses :: [Bus] }

buses :: String -> [Bus]
buses = map (\(i, n) -> Bus (readInt n) i) . filter ((/= "x") . snd) . zip [0..] . splitOn "," 

schedule :: String -> Schedule
schedule s = case lines s of
  d:bs:_ -> Schedule (readInt d) (buses bs)
  _      -> error $ "Bad input: " ++ s

waitTime :: Int -> Int -> Int
waitTime upTo busId = ((upTo + busId) `div` busId) * busId

earliest :: Schedule -> Int
earliest (Schedule departureTime bs) = (totalTime - departureTime) * busId   
  where ids = map _id bs
        (totalTime, busId) = minimum $ zip (map (waitTime departureTime) ids) ids
  
sameTime :: Schedule -> Integer
sameTime (Schedule _ bs) =
  case chineseRemainder resi moduli of 
    Nothing -> error "chinese remainder does not exist"
    Just re -> re
  where resi = map (toInteger . _offset) bs
        moduli = map (toInteger . _id) bs

instance Challenge Schedule where
  parse = schedule
  partOne = show . earliest
  partTwo = show . sameTime