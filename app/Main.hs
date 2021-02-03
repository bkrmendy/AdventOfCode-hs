module Main where

import Challenge
import Elfcode
import Year2018.Day16

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2018/16.txt"
  let parsed = parse input :: ([Capture], [Opcode])
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
