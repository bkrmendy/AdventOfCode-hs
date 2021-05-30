{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Intcode
import Year2019.Day22
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2019/22.txt"
  let parsed = parse input :: [Technique]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
