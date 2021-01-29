module Main where

import Challenge
import Year2015.Day25

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2015/25.txt"
  let parsed = parse input :: (Int, Int)
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
