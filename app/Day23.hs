{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Day23 where
import           AocPrelude
import           Control.Monad    (forM, forM_)
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (UArray (UArray))
import           Data.Array.ST    (MArray (newArray), STUArray, freeze,
                                   readArray, writeArray)
import           Data.Char        (digitToInt, intToDigit)
import           Prelude          ()

import qualified Data.Array.Base

infiniteOrdering minValue maxValue = go
    where
        go i
            | i == minValue = maxValue : go maxValue
            | otherwise = (i - 1) : go (i - 1)
generalCycledOrdering minValue maxValue = take nValues . infiniteOrdering minValue maxValue
    where
        nValues = maxValue - minValue + 1

-- >>> generalCycledOrdering 1 5 4
-- [3,2,1,5,4]

-- doMove :: [Int] -> [Int]
generalMove cyclicOrdering (currentCup:rest) = newRest ++ [currentCup]
    where
        (grabbed, onTable) = splitAt 3 rest
        (destinationCup:_) = filter (not . (`member` grabbed)) (cyclicOrdering currentCup)
        (onTableLeft, _:onTableRight) = span (/= destinationCup) onTable
        newRest = onTableLeft ++ (destinationCup:(grabbed ++ onTableRight))

-- >>> generalMove (generalCycledOrdering 1 9) [3, 8, 9, 1, 2, 5, 4, 6, 7]
-- [2,8,9,1,5,4,6,7,3]

solutionFormat cups = take (n - 1) . tail . dropWhile (/= 1) $ cups ++ cups
    where
        n = length cups

-- >>> solutionFormat [8,3,7,4,1,9,2,6,5]
-- [9,2,6,5,8,3,7,4]

toMappedRepr numbers@(x:xs) = (x,  initedVector)
    where
        updateList = zip numbers (xs ++ [x])
        uninitVector = fromList @(Vector _) (0:numbers)
        initedVector = uninitVector // updateList

toMappedReprST :: [Int] -> ST s (Int, STUArray s Int Int)
toMappedReprST numbers@(x:xs) = do
    let minValue = minimum numbers
        maxValue = maximum numbers
    result <- newArray (minValue, maxValue) (-1)
    forM_ (zip numbers (xs ++ [x])) (uncurry (writeArray result))
    return (x, result)

fromMappedRepr (f, n) = f:(takeWhile (/= f) . tail . iterate (n!) $ f)

fromMappedReprST :: (Int, STUArray s Int Int) -> ST s [Int]
fromMappedReprST (f, n) = do
    frozen::(UArray Int Int) <- freeze n
    let result = f:(takeWhile (/= f) . tail . iterate (frozen Data.Array.Base.!) $ f)
    return result


stMappingId numbers = toMappedReprST numbers >>= fromMappedReprST


-- >>> (fromMappedRepr . toMappedRepr) [8,3,7,4,1,9,2,6,5]
-- [8,3,7,4,1,9,2,6,5]

-- >>> runST (stMappingId [8,3,7,4,1,9,2,6,5])
-- [8,3,7,4,1,9,2,6,5]

mappedMove:: (Int -> [Int]) -> (Int, Vector Int) -> (Int, Vector Int)
mappedMove cyclicOrdering mappedRepr@(f, n) = (newN!f, newN)
    where
        grabbed@[g1, _, g3] = take 3 . tail . fromMappedRepr $ mappedRepr
        (destinationCup:_) = filter (not . (`member` grabbed)) (cyclicOrdering f)
        updates = [(f, n ! g3), (destinationCup, g1), (g3, n ! destinationCup)]
        newN =  n // updates

mappedMoveST :: (Int -> [Int]) -> (Int, STUArray s Int Int) -> ST s (Int, STUArray s Int Int)
mappedMoveST cyclicOrdering (f, n) = do
    g1 <- readArray n f
    g2 <- readArray n g1
    g3 <- readArray n g2
    g4 <- readArray n g3
    let (destinationCup:_) = filter (not . (`member` [g1, g2, g3])) (cyclicOrdering f)
    ndc <- readArray n destinationCup
    writeArray n f g4
    writeArray n destinationCup g1
    writeArray n g3 ndc
    newF <- readArray n f
    return (newF, n)

-- >>> (fromMappedRepr . mappedMove (generalCycledOrdering 1 9) . toMappedRepr) [3, 8, 9, 1, 2, 5, 4, 6, 7]
-- [2,8,9,1,5,4,6,7,3]

-- >>> runST $ toMappedReprST [3, 8, 9, 1, 2, 5, 4, 6, 7] >>= mappedMoveST (generalCycledOrdering 1 9) >>= fromMappedReprST
-- [2,8,9,1,5,4,6,7,3]

applyM :: (Monad m) => (a -> m a) -> Int -> a -> m a
applyM _ 0 m = return m
applyM f n m = f m >>= applyM f (n - 1)

applyNMoves cyclicOrdering nMoves numbers = runST $ do
    mappedRepr <- toMappedReprST numbers
    processed <- applyM (mappedMoveST cyclicOrdering) nMoves mappedRepr
    fromMappedReprST processed

solution1 nMoves2 initialStr = (map intToDigit . solutionFormat $ endstate1, r2)
    where
        numbers = map digitToInt initialStr
        lastCup = 1_000_000
        numbers2 = numbers ++ [maxValue+1..lastCup]
        minValue = minimum numbers
        maxValue = maximum numbers
        cycledOrdering1 = generalCycledOrdering minValue maxValue
        cycledOrdering2 = generalCycledOrdering minValue lastCup
        endstate1 = applyNMoves cycledOrdering1 100 numbers
        endstate2 = applyNMoves cycledOrdering2 nMoves2 numbers2
        (_, endstate2mapped) = toMappedRepr endstate2
        s2f = endstate2mapped ! 1
        s2s = endstate2mapped ! s2f
        r2 = s2f * s2s

-- | Day 23
-- >>> solution1 1000 "389125467"
-- ("67384529",12)

-- >>> solution1 10_000_000 "389125467"
-- ("67384529",149245887792)


-- >>> solution1 10_000_000 "653427918"
-- ("76952348",72772522064)
