{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Year2015.Day14
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2015/14.txt"
  let parsed = parse input :: [[Int]]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
