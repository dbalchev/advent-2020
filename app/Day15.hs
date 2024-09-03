{-# LANGUAGE TupleSections #-}
module Day15 where
import           AocPrelude
import           Prelude    ()


solveFoldFn :: (HashMap Int Int, Int) -> (Int, Int) -> (HashMap Int Int, Int)
solveFoldFn (memoryMap, _) (turnNo, lastSpoken) = (newMemoryMap, justSpoken)
    where
        justSpoken = maybe 0 (turnNo -) (memoryMap !? lastSpoken)
        newMemoryMap = insert (justSpoken, turnNo) memoryMap
-- solveFoldFn (memoryMap, _) (turnNo, Just starting) = (insert (starting, turnNo) memoryMap, starting)
-- solveFoldFn (memoryMap, lastSpoken) (turnNo, Nothing) = (newMemoryMap, justSpoken)
--     where
--         justSpoken = maybe 0 (turnNo -) (memoryMap !? lastSpoken)
--         newMemoryMap = insert (justSpoken, turnNo) memoryMap

solve1IterationFunction :: (HashMap Int Int, Int, [Int]) -> (HashMap Int Int, Int, [Int])
solve1IterationFunction (memoryMap, turnNo, nextNumber:restNumbers) = (newMemoryMap, turnNo + 1, nextRestNumbers)
    where
        newMemoryMap = insert (nextNumber, turnNo) memoryMap
        nextRestNumbers = if null restNumbers then [speakThisNext] else restNumbers
        speakThisNext = maybe 0 (turnNo -) (memoryMap !? nextNumber)



-- numberSequence :: [Int] -> [Int]
numberSequence startingNumbers = map pickSpokenNumber $ iterate solve1IterationFunction (empty, 1, startingNumbers)
    where
        pickSpokenNumber (_, _, x:_) = x


-- >>> take 10 $ numberSequence [0, 3, 6]
-- [0,3,6,0,3,3,1,0,4,0]

solve1 = (!! 2019) . numberSequence

-- >>> map solve1 [[0, 3, 6], [1, 3, 2], [2, 1, 3], [1, 2, 3], [2, 3, 1], [3, 2, 1], [3, 1, 2]]
-- [436,1,10,27,78,438,1836]

-- >>> solve1 [12,1,16,3,11,0]
-- 1696
