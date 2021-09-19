module Year2020.Day21 where

import Challenge  
import Utils (word, parseL)
import Text.Parsec
import Data.List (sort, intercalate)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Food = Food { _ingredients :: S.Set String, _allergens :: S.Set String } deriving (Show)

food :: Parsec String () Food
food = Food <$> is <*> as
  where is = S.fromList <$> many1 (word <* space)
        as = S.fromList <$> between (char '(') (char ')') (string "contains " *> sepBy1 word (string ", "))

type Allergens = M.Map String (S.Set String)

allergensFood :: S.Set String -> String -> Allergens -> Allergens
allergensFood ingredients allergen as = case M.lookup allergen as of
  Nothing -> M.insert allergen ingredients as
  Just is -> M.insert allergen (S.intersection ingredients is) as

allergens :: [Food] -> Allergens
allergens = foldr go M.empty
  where go (Food i a) as = foldr (allergensFood i) as a

ingredients :: [Food] -> S.Set String
ingredients = foldr (S.union . _ingredients) S.empty

allergicIngredients :: Allergens -> S.Set String
allergicIngredients as = foldr S.union S.empty (M.elems as)

nonAllergicIngredientOccurrences :: S.Set String -> [Food] -> Int
nonAllergicIngredientOccurrences nonAllergicIngredients = foldr go 0
  where go (Food is _) acc = acc + S.size (S.intersection nonAllergicIngredients is)

partOneI :: [Food] -> Int
partOneI foods = nonAllergicIngredientOccurrences nonAllergicIngredients foods
  where allergenToIngredient    = allergens foods
        allIngredients          = ingredients foods
        nonAllergicIngredients  = S.difference allIngredients (allergicIngredients allergenToIngredient) 

type AllergenByIngredient = (String, String)

extractSingleIngredient :: Allergens -> String -> String -> Allergens
extractSingleIngredient as a i = noIngredient
  where noAllergen = M.delete a as
        noIngredient = M.fromList [(aa, S.delete i os) | (aa, os) <- M.assocs noAllergen] 

narrowIngredients :: Allergens -> [AllergenByIngredient]
narrowIngredients as | M.null as = []
narrowIngredients as = (a, i):narrowIngredients extracted
  where (a, [i]) = head [(aa, S.elems ii) | (aa, ii) <- M.assocs as, S.size ii == 1]
        extracted = extractSingleIngredient as a i
         
partTwoI :: [Food] -> String
partTwoI foods = intercalate ","
               . map snd
               . sort
               . narrowIngredients
               $ allergens foods 

instance Challenge [Food] where
  parse = parseL food
  partOne = show . partOneI
  partTwo = partTwoI