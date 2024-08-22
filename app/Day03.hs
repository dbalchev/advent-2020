module Day03 where
import           AocPrelude
import           Data.Bool     (bool)
import           Data.Foldable (Foldable (toList))
import           Prelude       ()


type Grid = Vector (Vector Char)

countTreeHits :: Grid -> Int
countTreeHits grid = go (toList grid) 0 0
    where
        go :: [Vector Char] -> Int -> Int -> Int
        go [] _ acc = acc
        go (line:lines) currentPos acc = go lines nextPos newAcc
            where
                nextPos = (currentPos + 3) `mod` length line
                newAcc = acc + bool 0 1 ((line ! currentPos) == '#')


solution inputText = countTreeHits inputGrid
    where
        inputGrid :: Grid
        inputGrid = (fromList . map (fromList . unpack) . lines) inputText

--- >>> runSolution solution (TestInput "03")
-- 7

--- >>> runSolution solution (RealInput "03")
-- 209
