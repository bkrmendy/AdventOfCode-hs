module Main where

import Challenge
import DaySixteen

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/daysixteen/input.txt"
  let parsed = parse input :: Binary
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
