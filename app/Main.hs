module Main where

import Challenge
import Intcode
import Year2019.Day16

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2019/16.txt"
  let parsed = parse input :: [Int]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
