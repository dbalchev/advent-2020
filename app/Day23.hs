{-# LANGUAGE TypeApplications #-}
module Day23 where
import           AocPrelude
import           Data.Char  (digitToInt, intToDigit)
import           Prelude    ()

generalCycledOrdering minValue maxValue i = cache ! (i - minValue)
    where
        nValues = length normalList
        go n = take nValues . tail . dropWhile (/= n) $ repList
        normalList = [maxValue, maxValue - 1..minValue]
        repList = normalList ++ normalList
        cache = fromList @(Vector _) . map go $ [minValue..maxValue]

-- >>> generalCycledOrdering 1 5 5
-- [4,3,2,1,5]

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

solution1 initialStr = map intToDigit . solutionFormat $ states !! 100
    where
        numbers = map digitToInt initialStr
        minValue = minimum numbers
        maxValue = maximum numbers
        cycledOrdering = generalCycledOrdering minValue maxValue
        doMove = generalMove cycledOrdering
        states = iterate doMove numbers

-- >>> solution1 "389125467"
-- "67384529"

-- >>> solution1 "653427918"
-- "76952348"
