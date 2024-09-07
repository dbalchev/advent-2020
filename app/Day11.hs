{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
module Day11 where
import           AocPrelude
import           Control.DeepSeq (deepseq)
import           Control.Monad   (guard)
import           Data.Bool       (bool)
import           Prelude         ()

type Grid = Vector (Vector Char)

countNeirbyOccupied :: Grid -> Int -> Int -> Int
countNeirbyOccupied grid i j = sum $ do
    let nRows = length grid
        nCols = length (grid ! 0)
    di <- [-1, 0, 1]
    dj <- [-1, 0, 1]
    guard $ di /= 0 || dj /= 0
    let ni = i + di
    let nj = j + dj
    guard $ 0 <= ni && ni < nRows
    guard $ 0 <= nj && nj < nCols
    return . bool 0 1 $ (grid ! ni ! nj == '#')

directions :: Vector (Int, Int)
directions = fromList [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

countSeenOccupied :: Grid -> Int -> Int -> Int
countSeenOccupied grid = go nextSeenMemo
    where
        go memo i j = sum $ map (\d -> bool 0 1 $ memo ! (d, i, j) == '#') [0..7]
        nRows = length grid
        nCols = length (grid ! 0)
        nextSeen direction i j
            | ni < 0 || nj < 0 || ni >= nRows || nj >= nCols = '.'
            | currentCell /= '.' = currentCell
            | otherwise = nextSeenMemo ! (direction, ni, nj)
            where
                currentCell = grid ! ni ! nj
                (di, dj) = directions ! direction
                ni = di + i
                nj = dj + j
        nextSeenMemo = fromList @(HashMap _ _) $ do
            directionIndex <- [0..7] :: [Int]
            i <- [0..(nRows - 1)]
            j <- [0..(nCols - 1)]
            return ((directionIndex, i, j), nextSeen directionIndex i j)

oneStep :: (Grid -> Int -> Int -> Int) -> Int -> Grid -> Grid
oneStep countScore threshold grid = deepseq result result
    where
        newRow :: Int -> Vector Char
        newRow i = fromList $ map (newGrid i) [0..(nCols - 1)]
        nRows = length grid
        nCols = length (grid ! 0)
        scoreForGrid = countScore grid
        -- debugInfo = [[scoreForGrid i j| j <- [0..(nCols - 1)]]|i <- [0..(nRows - 1)]]
        result = fromList @Grid . map newRow $ [0..(nRows - 1)]
        newGrid i j = case (grid ! i ! j, scoreForGrid i j) of
            ('.', _) -> '.'
            ('L', 0) -> '#'
            ('#', x) -> if x >= threshold then 'L' else '#'
            (c, _)   -> c

countFixPointOccupied stepFn initialGrid = sum . map (bool 0 1 . (=='#')) . concatMap toList . toList $ finalGrid
    where
        steps = iterate stepFn initialGrid
        (finalGrid, _):_ = filter (uncurry (==)) $ zip steps (tail steps)

solution input = (solution1, solution2)
    where
        initialGrid = fromList @Grid . map (fromList . unpack) . lines $ input
        solution1 = countFixPointOccupied (oneStep countNeirbyOccupied 4) initialGrid
        solution2 = countFixPointOccupied (oneStep countSeenOccupied 5) initialGrid


-- | Test Day 11
-- >>> runSolution solution (TestInput "11")
-- (37,26)

-- >>> runSolution solution (RealInput "11")
-- (2093,1862)

