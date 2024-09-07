{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
module Day09 where
import           AocPrelude
import           Control.Monad (guard)
import           Data.Either   (rights)
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

solve1FoldFn :: (HashSet Int, [Int]) -> (Int, Int) -> (HashSet Int, [Int])
solve1FoldFn (numbersSet, restResult) (dropX, testX) = (updatedNumbersSet, newResult)
    where
        updatedNumbersSet = insert testX . delete dropX $ numbersSet
        newResult = if isSumOf2 numbersSet testX then restResult else testX:restResult


solve2 numbers cumSum x = minimum desiredList +  maximum desiredList
    where
        cumSumToIndex = fromList @(HashMap Int Int) $ zip (toList cumSum) [0..]
        (i, j):_ = do
            ci <- [0..length cumSum - 1]
            cj <- maybe [] singleton $ cumSumToIndex !? ((cumSum ! ci) + x)
            guard $ ci + 1 < cj
            return (ci + 1, cj + 1)
        desiredList = slice i (j - i) numbers

solution contextSize input = (solution1, solution2)
    where
        numbers = fromList @(Vector _ ) . map fst . rights . map decimal . words $ input
        numbersList = toList numbers
        (_, [solution1]) = foldl solve1FoldFn (fromList $ take contextSize numbersList, []) (zip numbersList (drop contextSize numbersList))
        solution2 = solve2 numbers cumSum solution1
        cumSum = fromList @(Vector _ ) $ scanl1 (+) numbersList


-- | Test Day 09
-- >>> runSolution (solution 5) (TestInput "09")
-- (127,62)

-- >>> runSolution (solution 25) (RealInput "09")
-- (36845998,4830226)
