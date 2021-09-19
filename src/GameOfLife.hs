module GameOfLife (
    GameOfLife(neighbors, transition)
  , GameOfLifeMap
  , State (Active, Inactive)
  , step
) where
  
import Control.Applicative (empty)
import qualified Data.Set as S
  
data State a = Active a | Inactive a
  
class (Eq a, Ord a) => GameOfLife a where
  neighbors :: a -> [a]
  transition :: Int -> State a -> Maybe a
  
type GameOfLifeMap a = S.Set a                           

status :: (GameOfLife a) => a -> GameOfLifeMap a -> State a
status p cube | S.member p cube = Active p
              | otherwise = Inactive p
  
nextState :: (GameOfLife a) => a -> GameOfLifeMap a -> Maybe a
nextState p cube = transition (length realNeighbors) (status p cube)
  where realNeighbors = filter (`S.member` cube) (neighbors p)   
  
step :: (GameOfLife a) => GameOfLifeMap a -> GameOfLifeMap a
step cube = S.fromList $ do
  p <- S.elems cube
  point <- p:neighbors p
  case nextState point cube of
    Just po -> pure po
    Nothing -> empty

