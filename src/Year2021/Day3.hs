module Year2021.Day3 where

import Challenge
import Utils (transpose)

import Data.List (foldl')  
import Data.Maybe (mapMaybe)
  
parseBit :: String -> [Bool]
parseBit = mapMaybe go
  where go '0' = Just False
        go '1' = Just True
        go _   = Nothing

parseI :: String -> [[Bool]]
parseI = map parseBit . lines

bitCount :: [Bool] -> (Int, Int)
bitCount = go (0, 0)
  where go (falses, trues) [] = (falses, trues)
        go (falses, trues) (True:bits) = go (falses, trues + 1) bits
        go (falses, trues) (False:bits) = go (falses + 1, trues) bits

mostCommon, leastCommon :: [Bool] -> Bool
mostCommon bits = falses <= trues
  where (falses, trues) = bitCount bits 
leastCommon bits = falses > trues
  where (falses, trues) = bitCount bits   

gamaRate, epsilonRate :: [[Bool]] -> [Bool]
gamaRate diagnostics = map mostCommon flipped
  where flipped = transpose diagnostics
epsilonRate diagnostics = map leastCommon flipped
  where flipped = transpose diagnostics

binaryToDec :: [Bool] -> Int
binaryToDec = foldl' (\acc val -> 2 * acc + if val then 1 else 0) 0

powerConsumption :: [[Bool]] -> Int
powerConsumption diagnostics = gama * epsilon
  where gama = binaryToDec $ gamaRate diagnostics
        epsilon = binaryToDec $ epsilonRate diagnostics
        
bitsInPos :: Bool -> Int -> [[Bool]] -> [[Bool]]
bitsInPos bit pos = filter go
  where go bits = (bits !! pos) == bit        
        
type BitCriteria = [Bool] -> Bool

criteria :: BitCriteria -> Int -> [[Bool]] -> [Bool]
criteria _ _ [bit] = bit
criteria c p bits = criteria c (p + 1) ac
  where bc = c (transpose bits !! p)
        ac = bitsInPos bc p bits
       
oxygenGeneratorRating, co2ScrubberRating :: [[Bool]] -> [Bool]
oxygenGeneratorRating = criteria mostCommon 0
co2ScrubberRating = criteria leastCommon 0 
        
lifeSupportRating :: [[Bool]] -> Int
lifeSupportRating bits = oxyRating * co2Rating
  where oxyRating = binaryToDec $ oxygenGeneratorRating bits
        co2Rating = binaryToDec $ co2ScrubberRating bits

instance Challenge [[Bool]] where
  parse = parseI
  partOne = show . powerConsumption
  partTwo = show . lifeSupportRating

