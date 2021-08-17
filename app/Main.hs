{-# LANGUAGE CPP #-}
module Main where

import System.IO (stdout, BufferMode(NoBuffering), hSetBuffering, stdin)
import System.FilePath (takeDirectory, (</>))

import Year2019.Day25 (step)
import qualified Intcode as IC

next :: ([IC.ProcessState], String) -> ([IC.ProcessState], String)
next ([], "undo") = ([], "Cannot undo with no machines!")
next ([m], "undo") = ([m], "Cannot undo only one machine!")
next (_:ms, "undo") = (ms, "Undone!")
next (m:rest, input) = (m':m:rest, output)
  where (output, m') = step m input  


repl :: [IC.ProcessState] -> IO ()
repl ms = do
  input <- getLine
  let (ms', out) = next (ms, input) 
  putStrLn out
  repl ms'

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  input <- readFile $ baseDir </> "input/2019/25.txt"
  let parsed = IC.fromString input
      machine = IC.initializeProcess parsed []
  repl [machine]

main :: IO ()
main = runChallenge
