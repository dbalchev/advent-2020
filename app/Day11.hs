{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
module Day11 where
import           AocPrelude
import           Control.DeepSeq (deepseq)
import           Control.Monad   (guard)
import           Data.Bool       (bool)
import           Data.Foldable   (Foldable (toList))
import           Debug.Trace     (traceShow)
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
        go memo i j = sum $ map (\d -> bool 0 1 $ memo ! (d, i, j) == '#') [0..8]
        nRows = length grid
        nCols = length (grid ! 0)
        nextSeen direction i j
            | i < 0 || j < 0 || i >= nRows || j >= nCols = '.'
            | currentCell /= '.' = currentCell
            | otherwise = nextSeenMemo ! (direction, ni, nj)
            where
                currentCell = grid ! i ! j
                (di, dj) = directions ! direction
                ni = di + i
                nj = dj + j
        nextSeenMemo =  traceShow "nexSeenMemo" $ fromList @(HashMap _ _) $ do
            directionIndex <- [0..8] :: [Int]
            i <- [0..(nRows - 1)]
            j <- [0..(nCols - 1)]
            return ((directionIndex, i, j), nextSeen directionIndex i j)

oneStep :: (Grid -> Int -> Int -> Int) -> Int -> Grid -> Grid
oneStep countScore threshold grid = traceShow ("oneStep", debugInfo) $ deepseq result result
    where
        newRow :: Int -> Vector Char
        newRow i = fromList $ map (newGrid i) [0..(nCols - 1)]
        nRows = length grid
        nCols = length (grid ! 0)
        scoreForGrid = countScore grid
        debugInfo = [[scoreForGrid i j| j <- [0..(nCols - 1)]]|i <- [0..(nRows - 1)]]
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

solution input = map toList . toList . step . step $ initialGrid
    where
        initialGrid = fromList @Grid . map (fromList . unpack) . lines $ input
        solution1 = countFixPointOccupied (oneStep countNeirbyOccupied 4) initialGrid
        step = oneStep countSeenOccupied 5
        -- solution2 = countFixPointOccupied (oneStep countSeenOccupied 5) initialGrid


-- >>> runSolution solution (TestInput "11")
-- /workspaces/advent-2020/app/AocPrelude.hs:200:13-39: Non-exhaustive patterns in Just value

-- >>> runSolution solution (RealInput "11")
-- 2093

