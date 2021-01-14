module Main where

import Challenge
import DayTwentyThree

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/daytwentythree/input.txt"
  let parsed = parse input :: [Instruction]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
