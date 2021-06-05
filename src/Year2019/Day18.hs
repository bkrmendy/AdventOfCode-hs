module Year2019.Day18 where

import Challenge

import Prelude hiding (lookup)
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Map.Strict as M
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

subSetOf :: (Ord a) => S.Set a -> S.Set a -> Bool
a `subSetOf` b = all (`S.member` b) (S.elems a)

data KeyData = KeyData { _code :: Char
                       , _distance :: Int
                       , _deps :: S.Set Char
                       } deriving (Eq, Ord)


type KeyList = S.Set KeyData
type Key2Key = M.Map (Char, Char) Int

keyDistance :: (Char, Char) -> Key2Key -> Int
keyDistance edge keys
  = case M.lookup edge keys of
      Nothing -> error $ (show edge) ++ " not in key map!"
      Just ds -> ds
     

step :: Key2Key -> KeyList -> S.Set Char -> Char -> KeyData -> Int
step key2key keyData keys currentKey key = stepLength + walkFast key2key nextKeySet nextKeys nextKey
  where
    stepLength = keyDistance (currentKey, _code key) key2key
    nextKey = _code key
    nextKeys = (S.insert nextKey keys)
    nextKeySet = (S.delete key keyData)

availableKeys :: S.Set KeyData -> S.Set Char -> [KeyData]
availableKeys keyData keys = [ key | key <- S.elems keyData, (_deps key) `subSetOf` keys]

walkFast :: Key2Key -> KeyList -> S.Set Char -> Char -> Int
walkFast key2key keyData keys currentKey
  = case availableKeys keyData keys of
      [] -> 0
      aKeys -> minimum $ map (step key2key keyData keys currentKey) aKeys

lookup :: Grid -> (Int, Int) -> Maybe ((Int, Int), Tile)
lookup grid pos
  | A.inRange (A.bounds grid) pos = Just (pos, grid A.! pos)
  | otherwise = Nothing

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
       
     
allKeyDistances :: Grid -> Key2Key
allKeyDistances = undefined

allKeysList :: Grid -> KeyList
allKeysList = undefined

charToTile :: Char -> Tile
charToTile '.' = Open
charToTile '#' = Wall
charToTile '@' = Key '@'
charToTile a
  | isLower a = Key a
  | isUpper a = Door a
  | otherwise = error "Unrecognized tile"

gridFromString :: [String] -> Grid
gridFromString ls = A.array ((0, 0), (width, height)) [ ((w, h), charToTile (get w h)) | w <- [0..width], h <- [0..height]]
  where
    (width, height) = (length (head ls) - 1, length ls - 1)
    get = \w h -> (ls !! h) !! w
    
kickoff2 :: Grid -> Int
kickoff2 grid = walkFast key2key keyData keys '@'
  where
    key2key = allKeyDistances grid
    keyData = allKeysList grid
    keys = S.singleton '@'
    
instance Challenge Grid where
  parse = gridFromString . lines
  partOne = show . kickoff2