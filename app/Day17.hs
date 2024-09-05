{-# LANGUAGE TypeApplications #-}
module Day17 where
import           AocPrelude
import           Control.Monad  (guard)
import           Data.Bifunctor (Bifunctor (bimap))
import           Data.Bool      (bool)
import           Data.Foldable  (Foldable (toList))
import           Data.List      (partition)
import           Prelude        ()

deltas = [-1, 0, 1]

adjCoordinates currentPoint@(x, y, z) = do
    dx <- deltas
    dy <- deltas
    dz <- deltas
    let adjPoint = (x + dx, y + dy, z + dz)
    guard $ currentPoint /= adjPoint
    return adjPoint

readInput input = activatedPointsList
    where
        grid = fromList @(Vector _ ) . map (fromList @(Vector Char) . unpack) . lines $ input
        activatedPointsList = do
            (y, cubeRow) <- zip [0..] (toList grid)
            (x, cubeChar) <- zip [0..] (toList cubeRow)
            guard $ cubeChar == '#'
            return (x, y, 0)


-- >>> runSolution readInput (TestInput "17")
-- [(1,0,0),(2,1,0),(0,2,0),(1,2,0),(2,2,0)]

newCubeState False 3 = True
newCubeState True 2  = True
newCubeState True 3  = True
newCubeState _ _     = False

type Point = (Int, Int, Int)
oneCycle :: (HashSet Point, HashSet Point) -> (HashSet Point, HashSet Point)
oneCycle (oldActiveSet, oldChangedSet) = (newActive, newChanged)
    where
        countActiveAdj point = sum . map (bool 0 1 . (`member` oldActiveSet)) $ adjCoordinates point
        informedNewState point = newCubeState (point `member` oldActiveSet) (countActiveAdj point)
        (changedToActivated, changedToInactivated) =  bimap (fromList @(HashSet _)) (fromList @(HashSet _)) . partition informedNewState . toList $ oldChangedSet
        newActive = (oldActiveSet `difference` changedToInactivated) `union` changedToActivated
        newActivated = changedToActivated `difference` oldActiveSet
        newDeactivated = changedToInactivated `intersection` oldActiveSet
        newChanged = fromList . concatMap (concatMap adjCoordinates . toList) $ [newActivated, newDeactivated]

initialState :: [Point] -> (HashSet Point, HashSet Point)
initialState initialActive = (fromList initialActive, fromList (initialActive ++ concatMap adjCoordinates initialActive))

solution input = length cycle6
    where
        cycle0 = initialState . readInput $ input
        stateList = iterate oneCycle cycle0
        (cycle6, _) = stateList !! 6

-- >>> runSolution solution (TestInput "17")
-- 112

-- >>> runSolution solution (RealInput "17")
-- 291

