{-# LANGUAGE FlexibleInstances #-}

module DayFour where
import Challenge

import Text.Parsec as Parsec

import Data.List (sort, group, sortOn)
import Data.Ord
import Data.Char (ord, chr)

data Room = Room  { name      :: [String]
                  , sectorID  :: Int
                  , checksum  :: String
                  } deriving (Show)

parseSym :: Parsec.Parsec String () String
parseSym = Parsec.many1 Parsec.letter

parseNum :: Parsec.Parsec String () Int
parseNum = read <$> Parsec.many1 Parsec.digit

parseRoom :: Parsec.Parsec String () Room
parseRoom = do
  name <- Parsec.sepEndBy1 parseSym (Parsec.string "-")
  sectorID <- parseNum
  checksum <- Parsec.between (Parsec.string "[") (Parsec.string "]") parseSym
  return $ Room { name = name, sectorID = sectorID, checksum = checksum}

parseRooms :: Parsec.Parsec String () [Room]
parseRooms = Parsec.sepBy1 parseRoom Parsec.newline

parse' :: String -> [Room]
parse' input =
  case Parsec.parse parseRooms "" input of
    Left err -> error $ show err
    Right rooms -> rooms

real :: Room -> Bool
real room =
  let check = take 5 $ concatMap (take 1) $ sortOn (Data.Ord.Down . length) (group $ sort $ concat (name room))
   in checksum room == check

rotate :: Int -> Char -> Char
rotate n c =
  let num = ord c
      base = num - 97
      incremented = (base + n) `mod` 26
      re = incremented + 97
  in chr re

instance Challenge [Room] where
  parse = parse'
  partOne = show . foldl (\acc r -> acc + sectorID r) 0 . filter real
  partTwo = show . map (\room -> room { name = map (map (rotate (sectorID room))) (name room) })


-- 911
-- 885