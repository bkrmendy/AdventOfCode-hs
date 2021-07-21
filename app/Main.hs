{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Year2020.Day12
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2020/12.txt"
  let parsed = parse input :: [Action]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
