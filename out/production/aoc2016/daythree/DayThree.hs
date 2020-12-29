{-# LANGUAGE FlexibleInstances #-}
module DayThree where

import Challenge
import Utils (transpose)

import Data.List.Split (chunksOf)

data Triangle = Triangle { a :: Int, b :: Int, c :: Int} deriving (Show)

fromString :: String -> Triangle
fromString line =
  let (f:s:t:_) = words line
  in Triangle
    { a = read f
    , b = read s
    , c = read t
    }

fromWords :: [String] -> Triangle
fromWords (f:s:t:_) =
  Triangle 
    { a = read f
    , b = read s
    , c = read t
    }
      
valid :: Triangle -> Bool
valid tri = f && s && t
  where
    f = a tri + b tri > c tri
    s = b tri + c tri > a tri
    t = c tri + a tri > b tri

partOneI :: [Triangle] -> String
partOneI =
  show . foldl inner 0
  where
    inner :: Int -> Triangle -> Int
    inner i tri = if valid tri then succ i else i

parse1 :: String -> [Triangle]
parse1 = map fromString . lines

parse2 :: String -> [Triangle]
parse2 = map fromWords . chunksOf 3 . concat . transpose . map words . lines

instance Challenge [Triangle] where
  parse = parse2
  partOne _ = "TODO" 
  partTwo = partOneI