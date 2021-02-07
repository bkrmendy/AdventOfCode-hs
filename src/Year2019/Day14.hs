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
makeQuantity needed quantum
  | needed <= quantum = 1
  | otherwise = 1 + makeQuantity (needed - quantum) quantum

mergeWithAdd :: Map.Map String Int -> Map.Map String Int -> Map.Map String Int
mergeWithAdd mapA mapB = foldr merge mapA (Map.assocs mapB)
  where
    merge :: (String, Int) -> Map.Map String Int -> Map.Map String Int
    merge (k, v) m = Map.insert k (Map.findWithDefault 0 k m + v) m

basics :: Catalog -> Map.Map String (Int, Int)
basics catalog = Map.fromList elems
  where
    elems = [(element, (cost, quantum)) | (element, (quantum, [Element cost "ORE"])) <- Map.assocs catalog]

basicElementsFor :: Catalog -> Element -> Map.Map String Int
basicElementsFor catalog (Element quantity element)
  | _material (head elementsFromCatalog) == "ORE" = Map.singleton element quantity -- element is basic
  | otherwise = Map.map (* makeQuantity quantity q)
                $ foldr mergeWithAdd Map.empty
                $ map (basicElementsFor catalog) elementsFromCatalog
    where
      (q, elementsFromCatalog) = catalog Map.! element

consume :: Map.Map String Int -> Map.Map String (Int, Int) -> Int
consume ingredients basics = sum $ map consumeI (Map.assocs ingredients)
  where
    consumeI :: (String, Int) -> Int
    consumeI (element, quantity) =
      let (cost, quantum) = basics Map.! element
      in cost * makeQuantity quantity quantum

totalMaterialsNeeded :: Element -> Catalog -> Int
totalMaterialsNeeded element catalog = consume totalBasics (basics catalog)
    where
      totalBasics = basicElementsFor catalog element

instance Challenge Catalog where
  parse = Map.fromList . map (\(a, Element q e) -> (e, (q, a))). parseLines (sepBy1 parseI newline)
  partOne = show . totalMaterialsNeeded (Element 1 "FUEL")
