{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
module Day17 where
import           AocPrelude
import           Control.Monad  (guard)
import           Data.Bifunctor (Bifunctor (bimap))
import           Data.Bool      (bool)
import           Data.List      (partition)
import           Prelude        ()

deltas = [-1, 0, 1]

generalAdjCoordinates wDeltas currentPoint@(x, y, z, w) = do
    dx <- deltas
    dy <- deltas
    dz <- deltas
    dw <- wDeltas
    let adjPoint = (x + dx, y + dy, z + dz, w + dw)
    guard $ currentPoint /= adjPoint
    return adjPoint

adjCoordinates1 = generalAdjCoordinates [0]
adjCoordinates2 = generalAdjCoordinates deltas

readInput input = activatedPointsList
    where
        grid = fromList @(Vector _ ) . map (fromList @(Vector Char) . unpack) . lines $ input
        activatedPointsList = do
            (y, cubeRow) <- zip [0..] (toList grid)
            (x, cubeChar) <- zip [0..] (toList cubeRow)
            guard $ cubeChar == '#'
            return (x, y, 0, 0)


-- >>> runSolution readInput (TestInput "17")
-- [(1,0,0,0),(2,1,0,0),(0,2,0,0),(1,2,0,0),(2,2,0,0)]

newCubeState False 3 = True
newCubeState True 2  = True
newCubeState True 3  = True
newCubeState _ _     = False

type Point = (Int, Int, Int, Int)
oneCycle :: (Point -> [Point]) -> (HashSet Point, HashSet Point) -> (HashSet Point, HashSet Point)
oneCycle adjCoordinates = celuarAutomataMove adjCoordinates transitionFn
    where
        transitionFn a =  newCubeState a . sum . map (bool 0 1)

initialState :: (Point -> [Point]) -> [Point] -> (HashSet Point, HashSet Point)
initialState adjCoordinates initialActive = (fromList initialActive, fromList (initialActive ++ concatMap adjCoordinates initialActive))

generalSolve adjCoordinates initialActivated = length . fst $ stateList !! 6
    where
        stateList = iterate (oneCycle adjCoordinates) (initialState adjCoordinates (toList initialActivated))
solution input = (solution1, solution2)
    where
        initialActivated = fromList @(Vector _) . readInput $ input
        solution1 = generalSolve adjCoordinates1 initialActivated
        solution2 = generalSolve adjCoordinates2 initialActivated

-- | Test Day 17
-- >>> runSolution solution (TestInput "17")
-- (112,848)

-- >>> runSolution solution (RealInput "17")
-- (291,1524)

