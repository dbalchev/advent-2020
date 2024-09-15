
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Day21 where
import           AocPrelude
import           Control.Monad (guard)
import           Prelude       ()

parseLine line
    | [ingridientsStr, containsStr] <- splitOn " (" line
    , Just containsConcated <- stripPrefix "contains " containsStr >>= stripSuffix ")"
    = (fromList @(HashSet _ ) $ words ingridientsStr, fromList @(HashSet _ ) $ splitOn ", " containsConcated)

-- | Day 21 parseLine
-- >>> parseLine "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
-- (fromList ["kfcds","nhms","sqjhc","mxmxvkd"],fromList ["dairy","fish"])

parseInput = fromList @(Vector _ ) .map parseLine . lines

solution input = solution1
    where
        parsedInput = parseInput input
        allIngredients = fromList @(HashSet _) . concatMap (toList . fst) . toList $ parsedInput
        allAlergens = fromList @(HashSet _) . concatMap (toList . snd) . toList $ parsedInput
        alergenToMealIngredients :: HashMap Text (Vector (HashSet Text))
        alergenToMealIngredients = collectByFirst $ do
            (ingredients, alergens) <- toList parsedInput
            alergen <- toList alergens
            return (alergen, ingredients)
        alergenSources = fromList @(HashSet _) $ do
            mealIngredients <- elems alergenToMealIngredients
            let currentSources = foldl1 intersection mealIngredients
            toList currentSources
        cleanIngredients = allIngredients `difference` alergenSources
        solution1 = length $ do
            (ingredients, _) <- toList parsedInput
            ingredient <- toList ingredients
            guard $ ingredient `member` cleanIngredients
            return ingredient

-- | Day 21
-- >>> runSolution solution (TestInput "21")
-- 5

-- >>> runSolution solution (RealInput "21")
-- 2573

