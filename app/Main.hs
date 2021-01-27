module Main where

import Challenge
import Year2015.Day13

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2015/13.txt"
  let parsed = parse input :: [Preference]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
