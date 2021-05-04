module Year2017.Day24 where
import            Challenge
import            Utils (readInt)

import            Control.Monad.State
import            Data.List.Split (splitOn)
import qualified  Data.Set as S

type Connector = (Int, Int)

connector :: String -> Connector
connector line = (readInt a, readInt b)
  where [a, b] = splitOn "/" line

newtype SearchState = SearchState {_cs :: S.Set Connector }

outPin :: Int -> Connector -> Maybe Int
outPin pi (i, o)
  | pi == i = Just o
  | pi == o = Just i
  | otherwise = Nothing

walk :: Int -> State SearchState [[Connector]]
walk pinIn = do
  cs <- gets _cs
  forM (S.elems cs) $ \conn -> do
    case outPin pinIn conn of
      Nothing -> return [conn]
      Just out -> do
          modify' $ \s -> s { _cs = S.delete conn (_cs s)}
          ps <- walk out
          return $ concatMap (conn:) ps

bridges :: State SearchState [[Connector]]
bridges = do
  (SearchState s) <- get
  zs <- pure $ length [() | (0, _) <- S.elems s]
  if zs < 1
    then return []
    else do
      paths <- walk 0
      (++) paths <$> bridges

strength :: [Connector] -> Int
strength = sum . concatMap go
  where go (a, b) = [a, b]

pt1 :: [Connector] -> Int
pt1 cs = maximum . map strength $ evalState bridges (SearchState $ S.fromList cs)

instance Challenge [Connector] where
  parse = map connector . lines
  partOne = show . pt1
