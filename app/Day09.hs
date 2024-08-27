{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
module Day09 where
import           AocPrelude
import           Control.Monad (guard)
import           Data.Either   (rights)
import           Data.Foldable (Foldable (toList))
import           Prelude       ()

isSumOf2 numbersSet x = (not . null) sums
    where
        sums = do
            a <- toList numbersSet
            let xma = x - a
            guard $ x /= xma
            guard $ xma `member` numbersSet
            return (a, xma)

-- >>> isSumOf2 (fromList @(HashSet Int) [35, 20, 15, 25, 47]) (40::Int)
-- True

solve1 :: [Int] -> HashSet Int -> [Int] -> [Int]
solve1 _ _ []                                   = []
solve1 (dropX:dropXs) numbersSet (testX:testXs)
    | isSumOf2 numbersSet testX = nextResult
    | otherwise = testX : nextResult
    where
        updatedNumbersSet = insert testX . delete dropX $ numbersSet
        nextResult = solve1 dropXs updatedNumbersSet testXs

solution contextSize input = solution1
    where
        numbers = fromList @(Vector _ ) . map fst . rights . map decimal . words $ input
        numbersList = toList numbers
        solution1 = solve1 numbersList (fromList $ take contextSize numbersList) (drop contextSize numbersList)

-- >>> runSolution (solution 5) (TestInput "09")
-- [127]

-- >>> runSolution (solution 25) (RealInput "09")
-- [36845998]
