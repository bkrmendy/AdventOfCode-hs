module Year2017.Day12 where
import            Challenge
import            Utils (int, parseL)

import            Data.List (partition)
import            Control.Monad.State
import            Text.Parsec hiding (State)
import qualified  Data.Map.Strict as M
import qualified  Data.Set as S

type Village = M.Map Int [Int]

pHouse :: Parsec String () (Int, [Int])
pHouse = (,) <$> int <*> (string " <-> " *> sepBy1 int (string ", "))

type Group = S.Set Int
data BFSState = BFSState { _village :: Village, _seen :: S.Set Int, _queue :: [Int] }

bfs :: State BFSState Group
bfs = do
  (BFSState v seen q) <- get
  if null q
    then return S.empty
    else do
      top <- pure $ head q
      modify' $ \s -> s { _queue = tail q }
      if top `S.member` seen
        then bfs
        else do
          neighbors <- pure $ M.findWithDefault [] top v
          modify' $ \s -> s { _queue = (tail q) ++ neighbors }
          modify' $ \s -> s { _seen = S.insert top seen }
          S.insert top <$> bfs
  

pickUnvisited :: State BFSState (Maybe Int)
pickUnvisited = do
  (BFSState v seen _) <- get
  unVisited <- pure $ [h | h <- M.keys v, not (h `S.member` seen)]
  case unVisited of
    [] -> return Nothing
    (h:_) -> return (Just h)

walkVillage :: State BFSState [Group]
walkVillage = do
  g <- bfs
  next <- pickUnvisited
  case next of
    Nothing -> return [g]
    Just h -> do
      modify' $ \s -> s { _queue = [h] }
      (:) g <$> walkVillage
  

groupOf :: Int -> [Group] -> (Maybe (S.Set Int), [Group])
groupOf e gs = case partition (e `S.member`) gs of
  ([g], rest) -> (Just g, rest)
  (_, rest) -> (Nothing, rest)  

pt1 :: Village -> Int
pt1 v = get . groupOf 0 $ evalState walkVillage (BFSState v S.empty [0])
  where get (Just g, _) = S.size g
  
pt2 :: Village -> Int
pt2 v =  length $ evalState walkVillage (BFSState v S.empty [0])

instance Challenge Village where
  parse = M.fromList . parseL pHouse
  partOne = show . pt1
  partTwo = show . pt2


