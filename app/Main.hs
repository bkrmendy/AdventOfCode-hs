{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Intcode
import Year2020.Day5
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2020/5.txt"
  let parsed = parse input :: [String]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
