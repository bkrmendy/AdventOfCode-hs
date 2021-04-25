{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Year2017.Day17
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2017/17.txt"
  let parsed = parse input :: Int
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
