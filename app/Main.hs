module Main where

import Challenge
import qualified Intcode as IC
import Year2019.Day2

runChallenge :: IO ()
runChallenge = do
  input <- readFile "/Users/prezi/IdeaProjects/aoc2016/input/2019/2.txt"
  let parsed = parse input :: IC.Program
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
