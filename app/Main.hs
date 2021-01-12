module Main where

import Challenge
import DaySeventeen

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/src/challenges/dayseventeen/input.txt"
  let parsed = parse input :: Passcode
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
