{-# LANGUAGE FlexibleInstances #-}
module Year2019.Day14 where
import Challenge
import Utils
import Text.Parsec
import qualified Data.Map as Map

data Element = Element { _quantity :: Int, _material :: String } deriving (Show, Eq, Ord)

type Catalog = Map.Map String (Int, [Element])
type OreCatalog = Map.Map Element Int

parseElement :: Parsec String () Element
parseElement = Element <$> (int <* char ' ') <*> many1 letter

parseI :: Parsec String () ([Element], Element)
parseI = (,) <$> sepBy1 parseElement (string ", ") <*> (string " => " *> parseElement)

makeQuantity :: Int -> Int -> Int
makeQuantity needed quantum = ceiling (toRational needed / toRational quantum)

amountOfOreNeeded :: [Element] -> Map.Map String Int -> Catalog -> Int
amountOfOreNeeded [] _ _ = 0
amountOfOreNeeded (Element amount "ORE":workList) cache catalog = amount + amountOfOreNeeded workList cache catalog
amountOfOreNeeded (Element amount name:workList) cache catalog
  | amountFromCache >= amount = amountOfOreNeeded workList (updateCache name (amountFromCache - amount)) catalog
  | otherwise =
      amountOfOreNeeded (workList ++ componentsAdjusted) (updateCache name (max 0 $ quantum * quantity - amountNeeded)) catalog
  where
    amountFromCache = Map.findWithDefault 0 name cache
    amountNeeded = amount - amountFromCache
    updateCache ename eamount = Map.insert ename eamount cache
    (quantum, components) = catalog Map.! name
    quantity = makeQuantity amountNeeded quantum
    componentsAdjusted = map (\(Element q n) -> Element (q * quantity) n) components
    
maxFuelFrom :: Int -> Int -> Catalog -> Int
maxFuelFrom ore fuel catalog
  | newFuel == fuel = fuel
  | otherwise = maxFuelFrom ore newFuel catalog
    where
      newFuel = round (toRational ore / toRational (amountOfOreNeeded [Element fuel "FUEL"] Map.empty catalog) * toRational fuel)

instance Challenge Catalog where
  parse = Map.fromList . map (\(es, Element amount name) -> (name, (amount, es))). parseLines (sepBy1 parseI newline)
  partOne = show . amountOfOreNeeded [Element 1 "FUEL"] Map.empty
  partTwo = show . maxFuelFrom 1000000000000 1
