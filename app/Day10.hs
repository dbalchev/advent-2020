{-# LANGUAGE TypeApplications #-}
module Day10 where
import           AocPrelude
import           Data.Either   (rights)
import           Data.Foldable (Foldable (toList))
import           Data.List     (sort)
import           Prelude       ()


diffDistribution joltages = foldl (flip $ alter (Just . maybe 1 (+1))) empty (zipWith (-) joltageRestList joltageList)
    where
        joltageList@(_:joltageRestList) = toList joltages

dp2 :: Vector Int -> Integer
dp2 joltages = memo ! (length joltages - 1)
    where
        go :: Int -> Integer
        go 0 = 1
        go n = sum . map (memo !) . takeWhile (\i -> (joltages ! i) >= threshold) $ [(n-1), (n-2)..0]
            where
                threshold = joltages ! n - 3
        memo = fromList @(Vector Integer) . map go $ [0..(length joltages - 1)]

solution input = (diffs ! 1 * diffs ! 3, dp2 joltages)
    where
        inputJoltages = fromList @(Vector Int) . (0:) . sort . map fst . rights . map decimal . words $ input
        joltages = snoc inputJoltages (3 + maximum inputJoltages)
        diffs = diffDistribution joltages

-- >>> runSolution solution (TestInput "10.1")
-- (35,8)

-- >>> runSolution solution (TestInput "10.2")
-- (220,19208)

-- >>> runSolution solution (RealInput "10")
-- (1876,14173478093824)
