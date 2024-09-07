{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE NumericUnderscores #-}
module Day15 where
import           AocPrelude
import           Data.Array.IO.Internals (IOUArray (IOUArray))
import           Data.Array.MArray       (MArray (newArray), readArray,
                                          writeArray)
import           Prelude                 ()



solve1IterationFunction :: (HashMap Int Int, Int, [Int]) -> (HashMap Int Int, Int, [Int])
solve1IterationFunction (!memoryMap, !turnNo, !nextNumber:restNumbers) = (newMemoryMap, turnNo + 1, nextRestNumbers)
    where
        newMemoryMap = insert (nextNumber, turnNo) memoryMap
        nextRestNumbers = if null restNumbers then [speakThisNext] else restNumbers
        speakThisNext = maybe 0 (turnNo -) (memoryMap !? nextNumber)

-- numberSequence :: [Int] -> [Int]
numberSequence startingNumbers = map pickSpokenNumber $ iterate solve1IterationFunction (empty, 1, startingNumbers)
    where
        pickSpokenNumber (_, _, x:_) = x

monadicSolution array (turnNo, nextNumber:restNumbers) = do
    readValue <- readArray array nextNumber
    let speakThisNext = if readValue == -1 then 0 else turnNo - readValue
        nextRestNumbers = if null restNumbers then [speakThisNext] else restNumbers
    writeArray array nextNumber turnNo
    return (turnNo + 1, nextRestNumbers)


nRepSequence :: Int -> (a -> IO a) -> a -> IO a
nRepSequence 0 _ m = return m
nRepSequence n f m = f m >>= nRepSequence (n -1) f
monadicNumberSequence nRepeats startingNumbers = do
    initialArray <- newArray (0, 30_000_000) (-1) :: IO (IOUArray Int Int)
    let
        action :: (Int, [Int]) -> IO (Int, [Int])
        action = monadicSolution initialArray
    nRepSequence nRepeats action (1, startingNumbers)



-- >>> take 50 $ numberSequence [0, 3, 6]
-- [0,3,6,0,3,3,1,0,4,0,2,0,2,2,1,8,0,5,0,2,6,18,0,4,15,0,3,21,0,3,3,1,17,0,5,17,3,6,17,3,3,1,10,0,10,2,26,0,4,25]

solve1 = (!! 2019) . numberSequence

solve1m = (head . snd <$>) <$> monadicNumberSequence 2019

-- | Test Day 15 2
-- >>> solve1m [0, 3, 6]
-- 436


solve2 = (!! (30000000 - 1)) . numberSequence
solve2m = (head . snd <$>) <$> monadicNumberSequence (30_000_000 - 1)

-- | Test Day 15
-- >>> map solve1 [[0, 3, 6], [1, 3, 2], [2, 1, 3], [1, 2, 3], [2, 3, 1], [3, 2, 1], [3, 1, 2]]
-- [436,1,10,27,78,438,1836]

-- >>> solve2m [0, 3, 6]
-- 175594

-- >>> solve1 [12,1,16,3,11,0]
-- 1696

-- >>> solve2m [12,1,16,3,11,0]
-- 37385

