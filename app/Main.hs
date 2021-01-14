module Main where

import Challenge
import DayTwentyOne

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/daytwentyone/input.txt"
  let parsed = parse input :: [Scramble]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
