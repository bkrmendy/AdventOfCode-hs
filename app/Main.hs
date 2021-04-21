{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Year2017.Day13
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2017/13.txt"
  let parsed = parse input :: Firewall
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
