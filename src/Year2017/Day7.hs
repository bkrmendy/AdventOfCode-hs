{-# LANGUAGE TupleSections #-}
module Year2017.Day7 where
import Challenge
import Utils (int, parseL, allEqual)
import qualified Data.Map as M
import Text.Parsec
import Data.List (find, nubBy)
import Data.Maybe (mapMaybe)
import Control.Monad.Reader

data Node = Node { _weight :: Int, _children :: [String] } deriving Show
intermediate = Node
leaf w = Node w []

type Nodes = [(String, Node)]
type Tower = M.Map String Node

pIntermediate :: Parsec String () (String, Node)
pIntermediate = (,) <$> (many1 letter <* space)
                    <*> (intermediate <$> (between (char '(') (char ')') int)
                                      <*> (string " -> " *> sepBy1 (many1 letter) (string ", ")))

pLeaf :: Parsec String () (String, Node)
pLeaf = (,) <$> (many1 letter <* space)
            <*> (leaf <$> (between (char '(') (char ')') int))

pNode :: Parsec String () (String, Node)
pNode = try pIntermediate <|> pLeaf

remove :: String -> Nodes -> Nodes
remove node nodes = mapMaybe doit nodes
  where
    doit (n, Node w c)
      | n == node = Nothing
      | node `elem` c = Just (n, Node w $ filter (/= node) c)
      | otherwise = Just (n, Node w c)

popEmpty :: Nodes -> Maybe (String, Nodes)
popEmpty nodes = case find (null . _children . snd) nodes of
    Nothing -> Nothing
    Just (popped, _) -> Just (popped, remove popped nodes)

root :: Nodes -> String
root nodes = case popEmpty nodes of
    Nothing -> error "No leaf nodes ???"
    Just (rootNode, []) -> rootNode
    Just (_, nextNodes) -> root nextNodes

weightOfChildren :: String -> Reader Tower [Int]
weightOfChildren n = do
  node <- asks (M.! n)
  mapM weight (_children node)

weight :: String -> Reader Tower Int
weight n = do
  node <- asks (M.! n)
  childWeights <- weightOfChildren n
  return (sum childWeights + _weight node)

balanced :: String -> Reader Tower Bool
balanced tower = do
  weights <- weightOfChildren tower
  return (allEqual weights)

unBalancedOne :: [(String, Int)] -> ((String, Int), Int)
unBalancedOne chs = ((unbalanced, ubWeight), commonWeight) -- disgusting
  where
    ((a, aw):(b, bw):_) = nubBy (\(_, x) (_, y) -> x == y) chs
    as = filter (== (a, aw)) chs
    ((unbalanced, ubWeight), commonWeight) = if length as > 1 then ((b, bw), aw) else ((a, aw), bw)


correction :: String -> Reader Tower Int
correction n = do
  node <- asks (M.! n)
  childrenBalanced <- mapM balanced (_children node)
  weights <- mapM weight (_children node)
  childWeights <- pure $ (zip (_children node) weights)
  ((unbalanced, ubWeight), commonWeight) <- pure $ unBalancedOne (zip (_children node) weights)
  if not (allEqual childWeights) && and childrenBalanced
    then do
      unBalancedNode <- asks (M.! unbalanced)
      return $ (_weight unBalancedNode) - (abs $ ubWeight - commonWeight)
    else
      correction unbalanced

partTwoI :: Tower -> Int
partTwoI tower = runReader (correction rootNode) tower
  where rootNode = root $ M.assocs tower

instance Challenge Nodes where
  parse = parseL pNode
  partOne = show . root
  partTwo = show . partTwoI . M.fromList


