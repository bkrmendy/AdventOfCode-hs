-- All notations and functions from https://www.redblobgames.com/grids/hexagons/
-- using cube coordinates
module Year2017.Day11 where
import Challenge
import Utils (parseLines)
import Text.Parsec hiding (State)
import Control.Monad.State

data CubeCoord = CubeCoord { _x :: Int, _y :: Int, _z :: Int }
data WalkState = WalkState { _pos :: CubeCoord, _max :: Int }

type StepFn = CubeCoord -> CubeCoord

pStep :: Parsec String () StepFn
pStep = try (step  1  0 -1 <$ string "ne")
    <|> try (step -1  1  0 <$ string "nw")
    <|> try (step  0  1 -1 <$ string "n")
    <|> try (step -1  0  1 <$ string "sw")
    <|> try (step  1 -1  0 <$ string "se")
    <|> try (step  0 -1  1 <$ string "s")

step :: Int -> Int -> Int -> StepFn
step dx dy dz (CubeCoord x y z) = CubeCoord (x + dx) (y + dy) (z + dz)

walk :: [StepFn] -> State WalkState WalkState
walk steps = do
  forM_ steps $ \s -> do
    (WalkState pos dst) <- get
    nextPos <- pure $ s pos
    nextDst <- pure $ max dst (distanceFromOrigin nextPos)
    put (WalkState nextPos nextDst)
  get

distanceFromOrigin :: CubeCoord -> Int
distanceFromOrigin (CubeCoord x y z) = maximum [abs x, abs y, abs z]

solve :: [StepFn] -> WalkState
solve steps = evalState (walk steps) (WalkState (CubeCoord 0 0 0) 0)

instance Challenge [StepFn] where
  parse = parseLines (sepBy1 pStep (char ','))
  partOne = show . distanceFromOrigin . _pos . solve
  partTwo = show . _max . solve
