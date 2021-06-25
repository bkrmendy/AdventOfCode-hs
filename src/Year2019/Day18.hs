{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Year2019.Day18 where

import Challenge

import Prelude hiding (lookup)
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Data.Char (isLower, isUpper, isDigit, toUpper)
import           Data.Maybe (catMaybes)
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity

data Tile
  = Open
  | Door Char
  | Key Char
  | Wall
  deriving Show

type Coord = (Int, Int)
type Grid = A.Array Coord Tile

subSetOf :: (Ord a) => S.Set a -> S.Set a -> Bool
a `subSetOf` b = all (`S.member` b) (S.elems a)

data KeyData = KeyData { _code :: Char, _deps :: S.Set Char } deriving (Eq, Ord, Show)

type KeyDeps = S.Set KeyData
type Key2Key = M.Map (Char, Char) Int
type Cache = M.Map (Char, S.Set Char) Int

newtype MazeM a = MazeM { unMazeM :: ReaderT Key2Key (StateT Cache Identity) a }
  deriving (Functor, Applicative, Monad, MonadReader Key2Key, MonadState Cache)

runMaze :: Key2Key -> Cache -> MazeM a -> a
runMaze key2key st (MazeM m) = runIdentity s
  where
    r = runReaderT m key2key
    s = evalStateT r st


keyDistance :: (Char, Char) -> Key2Key -> Int
keyDistance edge keys
  = case M.lookup edge keys of
      Nothing -> error $ show edge ++ " not in key map!"
      Just ds -> ds

step :: S.Set Char -> S.Set KeyData -> Char -> KeyData -> MazeM Int
step keys keyData currentKey key = do
  key2key <- ask
  let
    stepLength = keyDistance (currentKey, _code key) key2key
    nextKey = _code key
    nextKeys = S.insert (toUpper nextKey) keys
    nextKeySet = S.delete key keyData
  dst <- walkz nextKeys nextKeySet nextKey
  return $ stepLength + dst

availableKeys :: Char -> Key2Key -> S.Set KeyData -> S.Set Char -> [KeyData]
availableKeys cKey key2key keyData keys = do
  key <- S.elems keyData
  guard $ _deps key `subSetOf` keys
  guard $ _code key /= cKey
  guard $ (cKey, _code key) `M.member` key2key
  pure key

walkz :: S.Set Char -> S.Set KeyData -> Char -> MazeM Int
walkz keys keyData key = do
  cache <- get
  key2key <- ask
  if M.member (key, keys) cache
    then return $ cache M.! (key, keys)
    else do
      dst <- case availableKeys key key2key keyData keys of
          [] -> return 0
          aKeys -> minimum <$> mapM (step keys keyData key) aKeys
      modify' $ M.insert (key, keys) dst
      return dst
      

lookup :: Grid -> (Int, Int) -> Maybe (Coord, Tile)
lookup grid pos
  | A.inRange (A.bounds grid) pos = Just (pos, grid A.! pos)
  | otherwise = Nothing

neighbors :: Grid -> Coord -> [(Coord, Tile)]
neighbors grid (c, r) = catMaybes [lookup grid (c - 1, r), lookup grid (c + 1, r), lookup grid (c, r - 1), lookup grid (c, r + 1)]

type TileFound = (Tile, Int, Coord)

data BFSState = BFSState { _grid :: Grid, _seen :: S.Set Coord, _bkeys :: S.Set Char, _queue :: Seq.Seq TileFound }

data BFSStep a = BFSStep { _coord :: Coord, _tile :: Tile, _info :: a }

canEnter :: Tile -> Bool
canEnter Wall = False
canEnter _    = True

trail :: BFSStep a -> [(Char, a)]
trail (BFSStep _ (Key code) i) = [(code, i)]
trail _                               = []

type UpdateFn a = Coord -> Tile -> BFSStep a -> BFSStep a

updateDistance :: Coord -> Tile -> BFSStep Int -> BFSStep Int
updateDistance coord tile prev = BFSStep coord tile (_info prev + 1)

updateDeps :: Coord -> Tile -> BFSStep (S.Set Char) -> BFSStep (S.Set Char)
updateDeps coord t@(Door d) prev = BFSStep coord t (d `S.insert` (_info prev))
updateDeps coord t          prev = BFSStep coord t (_info prev)

bfs :: UpdateFn a -> Grid -> S.Set Coord -> [BFSStep a] ->  [(Char, a)]
bfs _      _    _    [] = []
bfs update grid seen (tile:rest) = trail tile ++ bfs update grid (S.insert (_coord tile) seen) (rest ++ do
  (c, t) <- neighbors grid (_coord tile)
  guard $ canEnter t
  guard $ not (c `S.member` seen)
  pure $ update c t tile)


allKeyDistances :: Grid -> Key2Key
allKeyDistances grid = M.fromList . concat $ do
  (coord, Key code) <- A.assocs grid
  pure $ map (\(c, d) -> ((code, c), d)) $ bfs updateDistance grid (S.singleton coord) [BFSStep coord (Key code) 0]

allKeysList :: Grid -> KeyDeps
allKeysList grid = S.fromList . map (\(c, d) -> KeyData c d) $ bfs updateDeps grid (S.singleton coord) [BFSStep coord (Key '1') (S.empty)]
  where coord = head $ [c | (c, Key k) <- A.assocs grid, isDigit k]

charToTile :: Char -> Tile
charToTile '.' = Open
charToTile '#' = Wall
charToTile a
  | isLower a || isDigit a = Key a
  | isUpper a = Door a
  | otherwise = error "Unrecognized tile"

gridFromString :: [String] -> Grid
gridFromString ls = A.array ((0, 0), (width, height)) [ ((w, h), charToTile (go w h)) | w <- [0..width], h <- [0..height]]
  where
    (width, height) = (length (head ls) - 1, length ls - 1)
    go = \w h -> (ls !! h) !! w

kickoff :: Char -> Grid -> Int
kickoff start grid = runMaze key2key M.empty (walkz keys keyData start)
  where
    key2key = allKeyDistances grid
    keyData = allKeysList grid
    keys = S.singleton start

kickoff2 :: Grid -> Int
kickoff2 grid = sum $ map (\k -> kickoff k grid) starts
  where starts = [k | Key k <- A.elems grid, isDigit k]
    
-- | In part two, map is updated manually
-- | Reason: cannot be bothered to add a generic impl
-- | TODO: add a generic impl
instance Challenge Grid where
  parse = gridFromString . lines
--  partOne = show . kickoff '1'
  partTwo = show . kickoff2