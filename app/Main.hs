module Main where

import Challenge
import DayFifteen

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/dayfifteen/input.txt"
  let parsed = parse input :: [DiskDescription]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
