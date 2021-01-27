module Main where

import Challenge
import Year2015.Day15

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2015/15.txt"
  let parsed = parse input :: [[Int]]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
