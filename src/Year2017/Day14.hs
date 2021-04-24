module Year2017.Day14 where
import            Challenge
import            KnotHash

import qualified  Data.HashSet as S
import qualified  Data.Sequence as Seq
import            Numeric      (readHex)
import            Text.Printf  (printf)
import            Control.Monad.State
import            Debug.Trace

-- ^ https://stackoverflow.com/a/48234931
hexToBin :: Char -> String
hexToBin c
  = case readHex [c] of
      (x,_):_ -> printf "%04b" (x::Int)

seeds :: String -> [String]
seeds ss = take 128 $ map (make ss) [0..]
  where make = \seed n -> seed <> "-" <> show n

type Grid = S.HashSet (Int, Int)

mkSet :: [String] -> Grid
mkSet ss = S.fromList [(w, h) | w <- [0..127], h <- [0..127], isUsed w h]
  where isUsed = \w h -> (ss !! h) !! w == '1'

fragSet :: String -> S.HashSet (Int, Int)
fragSet seed = mkSet $ map (concatMap hexToBin . knotHash) (seeds seed)

newtype BFSState = BFSState { _grid :: Grid }

neighbors :: (Int, Int) -> State BFSState [(Int, Int)]
neighbors (w, h) = do
  (BFSState g) <- get
  return $ [c | c <- [ (w,      h - 1)
                     , (w,      h + 1)
                     , (w + 1,  h)
                     , (w - 1,  h)
                     ]
              , S.member c g]

bfs :: Seq.Seq (Int, Int) -> State BFSState ()
bfs q
  | Seq.null q = return ()
  | otherwise = do
    left <- pure $ Seq.viewl q
    case left of
      Seq.EmptyL -> return ()
      (x Seq.:< rest) -> do
        (BFSState g) <- get
        ns <- neighbors x
        put (BFSState (S.delete x g))
        bfs (rest Seq.>< Seq.fromList ns)

nRegions :: State BFSState Int
nRegions = do
  (BFSState g) <- get
  if S.null g
    then return 0
    else do
      bfs $ Seq.singleton (head $ S.toList g)
      (1 +) <$> nRegions

partOneI :: Grid -> Int
partOneI = S.size

partTwoI :: Grid -> Int
partTwoI = evalState nRegions  . BFSState

instance Challenge Grid where
  parse = fragSet
  partOne = show . partOneI
  partTwo = show . partTwoI