module Main where

import Challenge
import Year2019.Day12

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2019/12.txt"
  let parsed = parse input :: [Moon]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
