module Main where

import Challenge
import DayTwentyTwo

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/daytwentytwo/input.txt"
  let parsed = parse input :: [Node]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
