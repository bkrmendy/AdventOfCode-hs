{-# LANGUAGE OverloadedStrings #-}

module DayOne where
import Challenge

import qualified Data.Text as T
import qualified Data.Set as S

data Direction = Bal | Jobb
data Instruction = Instruction Direction Int

newtype Instructions = Instructions [Instruction]

makeInstruction :: T.Text -> Instruction
makeInstruction instr =
  let steps = read $ T.unpack (T.tail instr) :: Int
  in
  case T.head instr of
    'L' -> Instruction Bal steps
    'R' ->  Instruction Jobb steps

data Heading = North | South | West | East

turn :: Heading -> Direction -> Heading
-- North
turn North Bal = West
turn North Jobb = East
-- West
turn West Bal = South
turn West Jobb = North
-- South
turn South Bal = East
turn South Jobb = West
-- East
turn East Bal = North
turn East Jobb = South

data Coord = Coord { x :: Int, y :: Int } deriving (Eq)
instance Ord Coord where
  compare (Coord x1 y1) (Coord x2 y2) =
     if x1 == x2
     then compare y1 y2
     else compare x1 x2

type State = (Heading, Coord)

move :: Coord -> Heading -> Int -> Coord
move (Coord x y) North n = Coord x     (y + n)
move (Coord x y) West n  = Coord (x - n) y
move (Coord x y) South n = Coord x     (y - n)
move (Coord x y) East n  = Coord (x + n) y

step :: State -> Instruction -> State
step (heading, position) (Instruction direction steps) =
  let newHeading = turn heading direction
  in (newHeading, move position newHeading steps)

distance :: Coord -> Int
distance (Coord x y) = abs x + abs y

dayOnePartOne :: Instructions -> String
dayOnePartOne (Instructions instructions) =
  let (heading, finish) = foldl step (North, Coord 0 0) instructions
   in show $ distance finish

data Navigation = Navigation
  { heading :: Heading
  , position :: Coord
  , places :: S.Set Coord
  }

move2 :: Navigation -> Heading -> Int -> Either Navigation Coord
move2 nav _ 0 = Left nav
move2 nav heading steps =
  let newPos = move (position nav) heading 1
      visited = S.member newPos (places nav)
      newNav = nav { position = newPos, places = S.insert newPos (places nav) }
  in
  if visited
  then Right newPos
  else move2 newNav heading (steps - 1)

step2 :: Navigation -> [Instruction] -> Coord
step2 nav (Instruction direction steps:is) =
  let newHeading = turn (heading nav) direction
  in
  case move2 (nav { heading = newHeading }) newHeading steps of
    Left nav -> step2 nav is
    Right coord -> coord

dayTwoPartTwo :: Instructions -> String
dayTwoPartTwo (Instructions instructions) =
  let start = Navigation { heading = North, position = Coord 0 0, places = S.empty }
  in show $ distance $ step2 start instructions

instance Challenge Instructions where
   parse = Instructions . map makeInstruction . T.splitOn ", " . T.pack
   partOne = dayOnePartOne
   partTwo = dayTwoPartTwo