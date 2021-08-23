module Year2019.Day25 (
  intcodeConsoleMain
) where
  
import qualified Intcode as IC
import Challenge

import System.IO (stdout, BufferMode(NoBuffering), hSetBuffering, stdin)
import System.FilePath ((</>))
import Data.Char (chr, ord)

toAscii :: [Int] -> String
toAscii = map chr

toInput :: String -> [Int]
toInput = map ord

step :: IC.ProcessState -> String -> (String, IC.ProcessState)
step machine input = IC.stepMachine machine $ do
  IC.addProcessInputs (toInput $ input ++ ['\n'])
  response <- IC.continueExecution
  return (toAscii response)
  
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

{-
  Solved manually
  The winning combo was:
  - space law space brochure
  - fixed point
  - sand
  - wreath
-}

intcodeConsoleMain :: String -> IO ()
intcodeConsoleMain baseDir = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  input <- readFile $ baseDir </> "input/2019/25.txt"
  let parsed = IC.fromString input
      machine = IC.initializeProcess parsed []
  repl [machine]

instance Challenge IC.Program where
  partOne _ = "16778274"
  partTwo _ = "Warp Drive Aligned!" 