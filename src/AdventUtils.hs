{-# LANGUAGE CPP #-}
module AdventUtils where
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

readInput :: String -> String -> IO String
readInput year day = readFile $ baseDir </> "input/" <> year <> "/" <> day <> ".txt"



