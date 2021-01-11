module Main where

import Challenge
import DayThirteen

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/daythirteen/input.txt"
  let parsed = parse input :: DesignersFavouriteNumber
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
