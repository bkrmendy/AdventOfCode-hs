module Main where

import Challenge
import DaySeven

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/dayseven/input.txt"
  print $ partOne (parse input :: [IPv7])
  print $ partTwo (parse input :: [IPv7])

main :: IO ()
main = runChallenge
