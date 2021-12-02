module Year2021.Day2 where
  
import Challenge
import Utils (readInt)

import Data.Maybe (mapMaybe)
import Data.List (foldl')

data Position = MkPosition { _forward :: Int, _up :: Int }

data Instruction
  = Forward Int
  | Down Int
  | Up Int

move :: Instruction -> Position -> Position
move (Forward n) (MkPosition f u) = MkPosition (f + n) u
move (Down n) (MkPosition f u) = MkPosition f (u + n)
move (Up n) (MkPosition f u) = MkPosition f (u - n)

aim :: Instruction -> (Int, Position) -> (Int, Position)
aim (Forward n) (a, MkPosition f u) = (a, MkPosition (f + n) (u + a * n))
aim (Down n) (a, position) = (a + n, position)
aim (Up n) (a, position) = (a - n, position)

parseLine :: String -> Maybe Instruction
parseLine line = case words line of
  "forward":n:_ -> Just $ Forward (readInt n)
  "up":n:_      -> Just $ Up (readInt n)
  "down":n:_    -> Just $ Down (readInt n)
  _             -> Nothing
  
solution :: Position -> Int
solution (MkPosition f u) = f * u  
  
partOneI :: Position -> [Instruction] -> Int
partOneI pos = solution . foldl' (flip move) pos

partTwoI pos = solution . snd . foldl' (flip aim) (0, pos)


instance Challenge [Instruction] where
  parse = mapMaybe parseLine . lines
  partOne = show . partOneI (MkPosition 0 0)
  partTwo = show . partTwoI (MkPosition 0 0)
  

