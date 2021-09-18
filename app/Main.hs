{-# LANGUAGE CPP #-}
module Main where

import System.FilePath (takeDirectory, (</>))

import Challenge
import Year2020.Day17

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2020/17.txt"
  let parsed = parse input :: [Dim2]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
