{-# LANGUAGE TypeApplications #-}
module Day11 where
import           AocPrelude
import           Control.Monad (guard)
import           Data.Bool     (bool)
import           Data.Foldable (Foldable (toList))
import           Prelude       ()

type Grid = Vector (Vector Char)

oneStep :: Grid -> Grid
oneStep grid = fromList @Grid . map newRow $ [0..(nRows - 1)]
    where
        newRow :: Int -> Vector Char
        newRow i = fromList $ map (newGrid i) [0..(nCols - 1)]
        nRows = length grid
        nCols = length (grid ! 0)
        countNeirbyOccupied i j = sum $ do
            di <- [-1, 0, 1]
            dj <- [-1, 0, 1]
            guard $ di /= 0 || dj /= 0
            let ni = i + di
            let nj = j + dj
            guard $ 0 <= ni && ni < nRows
            guard $ 0 <= nj && nj < nCols
            return . bool 0 1 $ (grid ! ni ! nj == '#')
        newGrid i j = case (grid ! i ! j, countNeirbyOccupied i j) of
            ('.', _) -> '.'
            ('L', 0) -> '#'
            ('#', x) -> if x >= 4 then 'L' else '#'
            (c, _)   -> c

solution input = solution1
    where
        initialGrid = fromList @Grid . map (fromList . unpack) . lines $ input
        steps = iterate oneStep initialGrid
        (finalGrid, _):_ = filter (uncurry (==)) $ zip steps (tail steps)
        solution1 = sum . map (bool 0 1 . (=='#')) . concatMap toList . toList $ finalGrid


-- >>> runSolution solution (TestInput "11")
-- 37

-- >>> runSolution solution (RealInput "11")
-- 2093

