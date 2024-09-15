
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Day21 where
import           AocPrelude
import           Control.Monad  (guard)
import           Data.List      (sort)
import           Data.Text.Lazy (intercalate)
import           Prelude        ()

parseLine line
    | [ingridientsStr, containsStr] <- splitOn " (" line
    , Just containsConcated <- stripPrefix "contains " containsStr >>= stripSuffix ")"
    = (fromList @(HashSet _ ) $ words ingridientsStr, fromList @(HashSet _ ) $ splitOn ", " containsConcated)

-- | Day 21 parseLine
-- >>> parseLine "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
-- (fromList ["kfcds","nhms","sqjhc","mxmxvkd"],fromList ["dairy","fish"])

parseInput = fromList @(Vector _ ) .map parseLine . lines

solution input = (solution1, alergenIngredientPairs)
    where
        parsedInput = parseInput input
        allIngredients = fromList @(HashSet _) . concatMap (toList . fst) . toList $ parsedInput
        allAlergens = fromList @(HashSet _) . concatMap (toList . snd) . toList $ parsedInput
        alergenToMealIngredients :: HashMap Text (Vector (HashSet Text))
        alergenToMealIngredients = collectByFirst $ do
            (ingredients, alergens) <- toList parsedInput
            alergen <- toList alergens
            return (alergen, ingredients)
        alergenPossibleIngredientMap = fromList @(HashMap _ _) $ do
            (alergen, mealIngredients) <- toKeyValuePairs alergenToMealIngredients
            let currentSources = foldl1 intersection mealIngredients
            return (alergen, currentSources)
        cleanIngredients = allIngredients `difference` (foldl1 union . elems $ alergenPossibleIngredientMap)
        solution1 = length $ do
            (ingredients, _) <- toList parsedInput
            ingredient <- toList ingredients
            guard $ ingredient `member` cleanIngredients
            return ingredient
        trivialSolution = trivailSolvePossibleIndices alergenPossibleIngredientMap
        alergenIngredientPairs = intercalate "," . map snd . sort $ do
            (alergen, ingredients) <- toKeyValuePairs trivialSolution
            let [ingredient] = toList ingredients
            return (alergen, ingredient)

-- | Day 21
-- >>> runSolution solution (TestInput "21")
-- (5,"mxmxvkd,sqjhc,fvjkl")

-- >>> runSolution solution (RealInput "21")
-- (2573,"bjpkhx,nsnqf,snhph,zmfqpn,qrbnjtj,dbhfd,thn,sthnsg")

