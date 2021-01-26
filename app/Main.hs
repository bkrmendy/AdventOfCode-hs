module Main where

import Challenge
import Year2016.Day1

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2016/1.txt"
  let parsed = parse input :: Instructions
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
