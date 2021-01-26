module Main where

import Challenge
import Day25

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/day25/input.txt"
  let parsed = parse input :: Int
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
