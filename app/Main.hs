{-# LANGUAGE CPP #-}
module Main where

import System.FilePath (takeDirectory, (</>))

import Challenge
import Year2020.Day22

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2020/22.txt"
  let parsed = parse input :: Puzzle
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
