module Year2019.Day18Slow where

import Challenge

import Prelude hiding (lookup)
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Sequence as Seq
import           Data.Foldable (toList)
import           Data.Char (isLower, isUpper, toLower)
import           Data.Maybe (catMaybes)
import           Control.Monad.State

data Tile
  = Open
  | Door Char
  | Key Char
  | Wall
  deriving Show

type Grid = A.Array (Int, Int) Tile

data WalkState = WalkState { _pos :: (Int, Int), _keys :: S.Set Char, _dst :: Int }

type KeyFound = (Char, Int, (Int, Int))

lookup :: Grid -> (Int, Int) -> Maybe ((Int, Int), Tile)
lookup grid pos
  | A.inRange (A.bounds grid) pos = Just (pos, grid A.! pos)
  | otherwise = Nothing

canOpen :: S.Set Char -> Char -> Bool
canOpen keys = (`S.member` keys) . toLower

canEnter :: S.Set Char -> Tile -> Bool
canEnter _ Open = True
canEnter _ Wall = False
canEnter _ (Key _) = True
canEnter keys (Door door) = canOpen keys door

neighbors :: Grid -> (Int, Int) -> [((Int, Int), Tile)]
neighbors grid (c, r) = catMaybes [lookup grid (c - 1, r), lookup grid (c + 1, r), lookup grid (c, r - 1), lookup grid (c, r + 1)]

type TileFound = (Tile, Int, (Int, Int))

data BFSState = BFSState { _grid :: Grid, _seen :: S.Set (Int, Int), _bkeys :: S.Set Char, _queue :: Seq.Seq TileFound }

dequeue :: Seq.Seq a -> (Maybe a, Seq.Seq a)
dequeue q = case Seq.viewl q of
  Seq.EmptyL -> (Nothing, q)
  a Seq.:< rest -> (Just a, rest)

enqueue :: Int -> [((Int, Int), Tile)] -> Seq.Seq TileFound
enqueue dst tiles = Seq.fromList (map go tiles)
  where
    go (pos, tile) = (tile, dst + 1, pos)

trail :: S.Set Char -> TileFound -> Seq.Seq KeyFound
trail keys (Key key, dst, pos)
  | key `S.member` keys        = Seq.empty
  | otherwise                  = Seq.singleton (key, dst, pos)
trail _    _                   = Seq.empty

canProceed :: S.Set Char -> S.Set (Int, Int) -> ((Int, Int), Tile) -> Bool
canProceed keys seen (pos, tile) = canEnter keys tile && not (S.member pos seen)

bfsGrid :: State BFSState (Seq.Seq KeyFound)
bfsGrid = do
  (BFSState grid seen keys q) <- get
  case dequeue q of
    (Nothing, _) -> return Seq.empty
    (Just (tile, dst, pos), nextQ) -> do
      neis <- pure $ filter (canProceed keys seen) $ neighbors grid pos
      put $ BFSState grid (S.insert pos seen) keys (nextQ Seq.>< enqueue dst neis)
      (Seq.><) (trail keys (tile, dst, pos)) <$> bfsGrid

keysFrom :: Grid -> WalkState -> [KeyFound]
keysFrom grid (WalkState pos keys _) = toList $ evalState bfsGrid initState
  where
    initState = BFSState grid (S.singleton pos) keys (Seq.singleton (Open, 0, pos))

proceed :: KeyFound -> WalkState -> WalkState
proceed (key, distance, position) (WalkState _ keys dst) = WalkState position (S.insert key keys) (dst + distance)

walk :: Grid -> WalkState -> Int
walk grid ws = case keysFrom grid ws of
  [] -> _dst ws
  kds -> minimum $ do
    kd <- kds
    pure (walk grid $ proceed kd ws)

charToTile :: Char -> Tile
charToTile '.' = Open
charToTile '@' = Open
charToTile '#' = Wall
charToTile a
  | isLower a = Key a
  | isUpper a = Door a
  | otherwise = error "Unrecognized tile"

gridFromString :: [String] -> Grid
gridFromString ls = A.array ((0, 0), (width, height)) [ ((w, h), charToTile (get w h)) | w <- [0..width], h <- [0..height]]
  where
    (width, height) = (length (head ls) - 1, length ls - 1)
    get = \w h -> (ls !! h) !! w
    
kickoff :: Grid -> Int
kickoff grid = walk grid (WalkState (8, 4) S.empty 0)
    
instance Challenge Grid where
  parse = gridFromString . lines
  partOne = show . kickoff

