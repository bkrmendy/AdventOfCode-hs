module Year2019.Day24 where

import Challenge
import Utils (countWhere)

import qualified Data.Set as S
import qualified Data.Array as A
import           Control.Monad.State


-- | Generic part of the Life implementation

data Bug = Live | Dead deriving (Eq, Ord, Show)

transition :: Bug -> Int -> Bug
transition Live ns = if ns == 1 then Live else Dead
transition Dead ns = if ns == 1 || ns == 2 then Live else Dead

type Eris = A.Array (Int, Int) Bug

fetch :: Eris -> (Int, Int) -> [Bug]
fetch e pos
  | A.inRange (A.bounds e) pos = [e A.! pos]
  | otherwise = []

type NeighborsFn = (Int, Int) -> [Bug]

step :: NeighborsFn -> Eris -> Eris
step ns e = A.array (A.bounds e) $ do
  (pos, health) <- A.assocs e
  let liveNeighbors = countWhere (Live ==) (ns pos)
  pure (pos, transition health liveNeighbors)


-- | Part 1 specific stuff
neighbors :: Eris -> NeighborsFn
neighbors e (x, y) = concat $ fetch e <$> [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

process :: Eris -> State (S.Set Eris) Eris
process e = do
   es <- get
   if e `S.member` es
    then return e
    else modify' (e `S.insert`) >> process (step (neighbors e) e)

biodiversity :: (Int, Int) -> Int
biodiversity (row, col) = 2 ^ (row * 5 + col)

pt1 :: Eris -> Int
pt1 = bidi . flip evalState S.empty . process
  where
    bidi = sum
              . map (biodiversity . fst)
              . filter ((==) Live . snd)
              . A.assocs


-- | Part 2 specific stuff

innerI :: Eris -> NeighborsFn
innerI e (1, 2) = [e A.! (0, c) | c <- [0 .. 4]]
innerI e (3, 2) = [e A.! (4, c) | c <- [0 .. 4]]
innerI e (2, 1) = [e A.! (r, 0) | r <- [0 .. 4]]
innerI e (2, 3) = [e A.! (r, 4) | r <- [0 .. 4]]
innerI _ _      = []

outerI :: Eris -> NeighborsFn
outerI e (r, c) = outerIRow r ++ outerICol c
  where
     outerIRow 0 = [e A.! (1, 2)]
     outerIRow 4 = [e A.! (3, 2)]
     outerIRow _ = []
     outerICol 0 = [e A.! (2, 1)]
     outerICol 4 = [e A.! (2, 3)]
     outerICol _ = []

neighbors2 :: Eris -> NeighborsFn
neighbors2 e (x, y) = concat $ do
   pos <- [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]
   guard $ pos /= (2,2)
   pure $ fetch e pos

neighborsPlutonian :: PlutonianSpace -> NeighborsFn
neighborsPlutonian (Top e d)      pos   = neighbors2 e pos ++ innerI (_eris d) pos
neighborsPlutonian (Bottom e u)   pos   = neighbors2 e pos ++ outerI (_eris u) pos
neighborsPlutonian (Middle e d u) pos   = neighbors2 e pos ++ innerI (_eris d) pos ++ outerI (_eris u) pos

data PlutonianSpace
  = Bottom { _eris :: Eris, _up :: PlutonianSpace }
  | Top { _eris :: Eris, _down :: PlutonianSpace }
  | Middle { _eris :: Eris, _down :: PlutonianSpace, _up :: PlutonianSpace }

processDown :: PlutonianSpace -> PlutonianSpace -> PlutonianSpace
processDown up b@(Bottom e _) =
  let
    nextEris = step (neighborsPlutonian b) e
    m = Middle nextEris (Bottom empty m) up
  in m
processDown up om@(Middle e d _) =
  let
    nextEris = step (neighborsPlutonian om) e
    m = Middle nextEris (processDown m d) up
  in m

processUp :: PlutonianSpace -> PlutonianSpace -> PlutonianSpace
processUp down t@(Top e _) =
  let
    nextEris = step (neighborsPlutonian t) e
    m = Middle nextEris down (Top empty m)
  in m
processUp down om@(Middle e _ u) =
  let
    nextEris = step (neighborsPlutonian om) e
    m = Middle nextEris down (processUp m u)
  in m

kickOffProcessPlutonian :: PlutonianSpace -> PlutonianSpace
kickOffProcessPlutonian om@(Middle e d u) = m
 where
    nextEris = step (neighborsPlutonian om) e
    m = Middle nextEris (processDown m d) (processUp m u)

countBugs :: Eris -> Int
countBugs e = countWhere (Live ==) es
  where es = [b | (p, b) <- A.assocs e, p /= (2, 2)]

nBugs :: PlutonianSpace -> Int
nBugs (Middle e down up) = countDown down + countBugs e + countUp up

countUp, countDown :: PlutonianSpace -> Int
countUp (Top e _) = countBugs e
countUp (Middle e _ u) = countBugs e + countUp u
countDown (Bottom e _) = countBugs e
countDown (Middle e d _) = countBugs e + countDown d

pt2 :: Int -> Eris -> Int
pt2 n eris = nBugs . last . take n $ iterate kickOffProcessPlutonian space
  where space = Middle eris (Bottom empty space) (Top empty space)

-- | PARSING

bug :: Char -> Bug
bug '#' = Live
bug '.' = Dead

empty :: Eris
empty = A.array ((0, 0), (4, 4)) $ do
  row <- [0..4]
  col <- [0..4]
  pure ((row, col), Dead)

populated :: [String] -> Eris
populated ls = A.array ((0, 0), (4, 4)) $ do
  row <- [0..4]
  col <- [0..4]
  pure ((row, col), bug $ ls !! row !! col)

-- | Challenge instance
instance Challenge Eris where
  parse = populated . lines
  partOne = show . pt1
  partTwo = show . pt2 201