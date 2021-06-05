module Year2019.Day21 where

import Challenge
import qualified Intcode as IC

import Prelude hiding (and, or, not)
import Control.Monad.Writer
import Data.Char (ord, chr)


data Register = A | B | C | D | E | F | G | H | I | T | J deriving Show
data Instr
  = AND Register Register
  | OR Register Register
  | NOT Register Register
  | WALK
  deriving Show

writable :: Register -> Bool
writable T = True
writable J = True
writable _ = False

instr :: (Register -> Register -> Instr) -> Register -> Register -> Writer String ()
instr con x y
  | writable y = do
      tell (show $ con x y)
      tell "\n"
  | otherwise = error $ show y ++ " not writable!"

and, or, not :: Register -> Register -> Writer String ()
and = instr AND
or  = instr OR
not = instr NOT

a, b, c, d, t, j, e, f, g, h , i :: Register
a = A
b = B
c = C
d = D
t = T
j = J
e = E
f = F
g = G
h = H
i = I

walk :: Writer String ()
walk = tell "WALK\n"

run :: Writer String ()
run = tell "RUN\n"

program :: [Int]
program = map ord . snd . runWriter $ do
  or a j
  and c j
  not j j
  and d j
  walk

program2 :: [Int]
program2 = map ord . snd . runWriter $ do
  or a j
  and b j
  and c j
  not j j
  and d j
  or e t
  or h t
  and t j
  run

solve :: [Int] -> IC.Program -> String
solve code source = show (last res)
  where res = IC.executeCode code source


instance Challenge IC.Program where
  parse = IC.fromString
  partOne = solve program
  partTwo = solve program2


