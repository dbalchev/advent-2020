module Day03 where
import           AocPrelude
import           Data.Bool     (bool)
import           Data.Foldable (Foldable (toList))
import           Prelude       ()


type Grid = Vector (Vector Char)

countTreeHits :: Grid -> (Int, Int) -> Int
countTreeHits grid (deltaI, deltaJ) = go 0 0 0
    where
        go :: Int -> Int -> Int -> Int
        go i j acc
            | i >= length grid = acc
            | otherwise = go (i + deltaI) nextPos newAcc
            where
                line = grid ! i
                nextPos = (j + deltaJ) `mod` length line
                newAcc = acc + bool 0 1 ((line ! j) == '#')


solution inputText = (solution1, solution2)
    where
        inputGrid :: Grid
        inputGrid = (fromList . map (fromList . unpack) . lines) inputText
        solution1 = countTreeHits inputGrid (1, 3)
        solution2 = product $ map (countTreeHits inputGrid) predefinedDeltas
        predefinedDeltas = [
            (1, 1),
            (1, 3),
            (1, 5),
            (1, 7),
            (2, 1)
            ]

--- >>> runSolution solution (TestInput "03")
-- (7,336)

--- >>> runSolution solution (RealInput "03")
-- (209,1574890240)
