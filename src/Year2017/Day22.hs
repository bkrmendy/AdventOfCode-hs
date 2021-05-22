module Year2017.Day22 where
import           Challenge
import qualified  Data.Map.Strict as M
import            Control.Monad.State

data Node = Clean | Infected | Weakened | Flagged deriving Eq
type Grid = M.Map (Int, Int) Node

parseI :: [String] -> Grid
parseI str = M.fromList [((r, c), get r c) | c <- [0..width - 1], r <- [0..height - 1]]
  where
    width = length (head str)
    height = length str
    get = \h w -> if (str !! h) !! w == '#' then Infected else Clean


data WalkState = WalkState { _pos :: (Int, Int) -- ^ position row, position column
                           , _dir :: (Int, Int) -- ^ delta row, delta column
                           , _grid :: Grid
                           , _infectionCount :: Int
                           }

turnI :: Node -> (Int, Int) -> (Int, Int)
turnI Clean     (dR, dC) = (-dC, dR)
turnI Infected  (dR, dC) = (dC, -dR)
turnI Weakened  (dR, dC) = (dR, dC)
turnI Flagged   (dR, dC) = (-dR, -dC)

turn :: State WalkState ()
turn = do
  (WalkState pos dir grid _) <- get
  let currentNode = M.findWithDefault Clean pos grid
  modify' $ \m -> m { _dir = turnI currentNode dir }

toggle :: Toggle -> State WalkState ()
toggle toggleRule = do
  (WalkState pos _ _ ic) <- get
  currentNode <- gets $ M.findWithDefault Clean pos . _grid
  nextNode <- pure $ toggleRule currentNode
  modify' $ \m -> m { _grid = M.insert pos nextNode (_grid m) }
  when (nextNode == Infected) $ do modify' $ \m -> m { _infectionCount = ic + 1 }


advance :: State WalkState ()
advance = do
  modify' $ \(WalkState (pR, pC) (dR, dC) g ic) -> WalkState (pR + dR, pC + dC) (dR, dC) g ic

walk :: Toggle -> Int -> State WalkState Int
walk toggleRule bursts = do
  forM_ [1..bursts] $ \_ -> do
    turn
    toggle toggleRule
    advance
  gets _infectionCount

type Toggle = Node -> Node

togglePt1 :: Toggle
togglePt1 Clean     = Infected
togglePt1 Infected  = Clean
togglePt1 Weakened  = Weakened
togglePt1 Flagged   = Flagged

togglePt2 :: Toggle
togglePt2 Clean     = Weakened
togglePt2 Weakened  = Infected
togglePt2 Infected  = Flagged
togglePt2 Flagged   = Clean


solve :: Toggle -> Int -> Grid -> Int
solve toggleRule bursts grid = evalState (walk toggleRule bursts) startingState
  where startingState = WalkState (12, 12) (-1, 0) grid 0

instance Challenge Grid where
  parse = parseI . lines
  partOne = show . solve togglePt1 10000
  partTwo = show . solve togglePt2 10000000
