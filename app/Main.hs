module Main where

import Challenge
import DayNineteen

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/daynineteen/input.txt"
  let parsed = parse input :: NElves
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
