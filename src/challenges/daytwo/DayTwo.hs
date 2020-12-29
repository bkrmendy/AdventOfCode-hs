{-# LANGUAGE OverloadedStrings #-}

module DayTwo where

import Challenge

data Direction = Fel | Le | Jobbra | Balra deriving (Show)

newtype Instructions = Instructions [[Direction]] deriving (Show)

toDirection :: Char -> Direction
toDirection 'L' = Balra
toDirection 'R' = Jobbra
toDirection 'U' = Fel
toDirection 'D' = Le

parseInstructions :: String -> Instructions
parseInstructions = Instructions . map (map toDirection) . lines

data Position = Position { row :: Int, column :: Int }

data KeyPad a = KeyPad
  { keyPad :: [[a]]
  ,  rows :: Int
  ,  columns :: Int
  }

positionToKeypad :: KeyPad a -> Position -> a
positionToKeypad keypad (Position row column)
  | row < rows keypad  && row >= 0 && column < columns keypad && column >= 0 =
      keyPad keypad !! row !! column
  | otherwise = error $ "out of bounds at " ++ show row ++ show column

clamp :: Int -> Int -> Int
clamp ceiling val = max 0 $ min (ceiling - 1) val

move :: KeyPad a -> Position -> Direction -> Position
move keypad (Position row col) Balra =
  Position row (clamp (columns keypad) (col - 1))
move keypad (Position row col) Jobbra =
  Position row (clamp (columns keypad) (col + 1))
move keypad (Position row col) Fel =
  Position (clamp (rows keypad) (row - 1)) col
move keypad (Position row col) Le =
  Position (clamp (rows keypad) (row + 1)) col

keyPad1 :: KeyPad String
keyPad1 = KeyPad
  { keyPad = [ [ "1", "2", "3" ]
             , [ "4", "5", "6" ]
             , [ "7", "8", "9" ]
             ]
  , rows = 3
  , columns = 3
  }

findDigit :: KeyPad a -> Position -> [Direction] -> (Position, a)
findDigit keypad pos dirs =
  let finalPos = foldl (move keypad) pos dirs
  in (finalPos, positionToKeypad keypad finalPos)

partOneI :: Instructions -> String
partOneI (Instructions is) =
  snd $ foldl inner (Position 1 1, "") is
  where
    inner :: (Position, String) -> [Direction] -> (Position, String)
    inner (sp, c) ds =
      let (fp, k) = findDigit keyPad1 sp ds
      in (fp, c ++ k)


data Button = X | I String

keyPad2 :: KeyPad Button
keyPad2 = KeyPad
  { keyPad = [ [X,      X,     I "1", X,            X]
             , [X,      I "2", I "3", I "4",        X]
             , [I "5",  I "6", I "7", I "8", I "9", X]
             , [X,      I "A", I "B", I "C",        X]
             , [X,      X,     I "D", X,            X]
             ]
  , rows = 5
  , columns = 5
  }
  
moveWithForbidden :: KeyPad Button -> Position -> Position -> Position
moveWithForbidden keypad def pos =
  case keyPad keypad !! row pos !! column pos of
    X -> def
    I _ -> pos
    
move2 :: KeyPad Button -> Position -> Direction -> Position
move2 keypad (Position row col) Balra =
  Position row (clamp (columns keypad) (col - 1))
move2 keypad (Position row col) Jobbra =
  Position row (clamp (columns keypad) (col + 1))
move2 keypad (Position row col) Fel =
  Position (clamp (rows keypad) (row - 1)) col
move2 keypad (Position row col) Le =
  Position (clamp (rows keypad) (row + 1)) col
  
findDigit2 :: KeyPad Button -> Position -> [Direction] -> (Position, Button)
findDigit2 keypad pos dirs =
  let finalPos = foldl inner pos dirs
  in (finalPos, positionToKeypad keypad finalPos)
  where
    inner p d = moveWithForbidden keypad p (move2 keypad p d)

partTwoI :: Instructions -> String
partTwoI (Instructions instructions) =
   snd $ foldl inner (Position 1 1, "") instructions
     where
       inner :: (Position, String) -> [Direction] -> (Position, String)
       inner (pos, button) dirs =
        let (finalPos, k) = findDigit2 keyPad2 pos dirs
        in case k of
          X -> error "X found"
          I key -> (finalPos, button ++ key) 
        

instance Challenge Instructions where
  parse = parseInstructions
  partOne = partOneI
  partTwo = partTwoI


