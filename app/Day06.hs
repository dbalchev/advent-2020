{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Day06 where
import           AocPrelude
import           Data.Foldable (Foldable (toList))
import           Data.List     (nub, sort)
import           Prelude       ()

solution inputText = (part1, part2)
    where
        part1 = sum $ map (length . nub . sort . concatMap unpack) $ toList answersPerGroup
        part2 = sum $ map (length . foldl1 intersection . map (fromList @(HashSet Char) . unpack) . toList) $ toList answersPerGroup
        answersPerGroup = fromList @(Vector _) . map (fromList @(Vector _ ) . splitOn "\n") . splitOn "\n\n" $ inputText

-- >>> runSolution solution (TestInput "06")
-- (11,6)

-- >>> runSolution solution (RealInput "06")
-- (6680,3117)
