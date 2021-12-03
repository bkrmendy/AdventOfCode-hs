{-# LANGUAGE CPP #-}
module Main where

import System.FilePath (takeDirectory, (</>))

import Challenge
import Year2021.Day3

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2021/3.txt"
  let parsed = parse input :: [[Bool]]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
