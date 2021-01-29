module Main where

import Challenge
import Year2015.Day24

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2015/24.txt"
  let parsed = parse input :: [Integer]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
