module Main where

import Challenge
import Day24

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/day24/input.txt"
  let parsed = parse input :: [String]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
