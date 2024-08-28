{-# LANGUAGE TypeApplications #-}
module Day10 where
import           AocPrelude
import           Data.Either   (rights)
import           Data.Foldable (Foldable (toList))
import           Data.List     (sort)
import           Prelude       ()


diffDistribution joltages = foldl foldFn empty (zipWith (-) joltageRestList joltageList)
    where
        joltageList@(_:joltageRestList) = toList joltages
        foldFn distributionMap delta = alter (Just . maybe 1 (+1)) delta distributionMap

solution input = diffs ! 1 * diffs ! 3
    where
        inputJoltages = fromList @(Vector Int) . (0:) . sort . map fst . rights . map decimal . words $ input
        joltages = snoc inputJoltages (3 + maximum inputJoltages)
        diffs = diffDistribution joltages


-- >>> runSolution solution (TestInput "10.1")
-- 35

-- >>> runSolution solution (TestInput "10.2")
-- 220

-- >>> runSolution solution (RealInput "10")
-- 1876
