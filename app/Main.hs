{-# LANGUAGE CPP #-}
module Main where

import System.FilePath (takeDirectory, (</>))

import Challenge
import Year2020.Day21

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2020/21.txt"
  let parsed = parse input :: [Food]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
