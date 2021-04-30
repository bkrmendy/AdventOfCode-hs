module Year2017.Day19 where
import           Challenge

import            Prelude             hiding (lookup)
import            Control.Monad.State
import            Data.List           (find)
import qualified  Data.Array as A

type Diagram = A.Array (Int, Int) Char

data Direction  = Direction { _dC :: Int, _dR :: Int } deriving Show
data Position   = Position  { _c  :: Int, _r  :: Int } deriving Show

data LookAhead = Letter Char | Cross | Blank | Road

data WalkState = WalkState Diagram Direction Position

mkMap :: [String] -> Diagram
mkMap ls = A.array ((0, 0), (width, height)) [((c, r), get c r) | c <- [0..width], r <- [0..height]]
  where
    width = maximum (map length ls) - 1
    height = length ls - 1
    get = \col row -> let r = ls !! row in if col < length r then r !! col else ' '

printMap :: Diagram -> [String]
printMap dia = [[dia A.! (w, h) | w <- [0..width]] | h <- [0..height]]
  where (_, (width, height)) = A.bounds dia

entryPoint :: Diagram -> Position
entryPoint dia = Position {_c = eCol, _r = eRow }
  where
    (_, (width, _)) = A.bounds dia
    (eCol, eRow) = head $ [(c, 0) | c <- [0..width], dia A.! (c, 0) == '|']

tileToLookAhead :: Char -> LookAhead
tileToLookAhead a | a `elem` ['A'..'Z'] = Letter a
tileToLookAhead t = case t of
  '+' -> Cross
  '|' -> Road
  '-' -> Road
  _   -> Blank

lookup :: Diagram -> (Int, Int) -> LookAhead
lookup dia coord =
  if A.inRange (A.bounds dia) coord
    then tileToLookAhead (dia A.! coord)
    else Blank

good :: LookAhead -> Bool
good Blank  = False
good _      = True

lookAhead :: State WalkState LookAhead
lookAhead = undefined

advance :: State WalkState ()
advance = do
  (WalkState dia (Direction dc dr) (Position c r)) <- get
  put (WalkState dia (Direction dc dr) (Position (c + dc) (r + dr)))

turn :: State WalkState ()
turn = do
  (WalkState dia (Direction dc dr) (Position c r)) <- get
  directions <- pure $ [(0, -1), (0, 1), (-1, 0), (1, 0)]
  (ndc, ndr) <- pure $ head [(cdc, cdr) | (cdc, cdr) <- directions, (cdc, cdr) /= (-dc, -dr), good (lookup dia (c + cdc, r + cdr))]
  put $ WalkState dia (Direction ndc ndr) (Position c r)

walk :: State WalkState [LookAhead]
walk = do
  (WalkState dia (Direction dc dr) (Position c r)) <- get
  let step = lookup dia (c + dc, r + dr)
  case step of
    Blank -> return [Blank]
    Cross -> advance >> turn >> (:) step <$> walk
    _     -> advance >> (:) step <$> walk

pt1 :: Diagram -> [Char]
pt1 dia = [l | Letter l <- evalState walk (WalkState dia (Direction 0 1) (entryPoint dia))]

pt2 :: Diagram -> Int
pt2 dia = length $ evalState walk (WalkState dia (Direction 0 1) (entryPoint dia))

instance Challenge Diagram where
  parse = mkMap . lines
  partOne = pt1
  partTwo = show . pt2