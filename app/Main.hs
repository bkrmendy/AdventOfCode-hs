module Main where

import Challenge
import DayEighteen

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/dayeighteen/input.txt"
  let parsed = parse input :: Row
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
