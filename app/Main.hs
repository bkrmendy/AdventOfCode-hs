module Main where

import Challenge
import DayOne

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/dayone/input.txt"
  print $ partOne (parse input :: Instructions)
  print $ partTwo (parse input :: Instructions)

main :: IO ()
main = runChallenge
