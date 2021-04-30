{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Year2017.Day19
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2017/19.txt"
  let parsed = parse input :: Diagram
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
