{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Day06 where
import           AocPrelude
import           Data.List  (nub, sort)
import           Prelude    ()

solution inputText = (part1, part2)
    where
        part1 = sum $ map (length . nub . sort . concatMap unpack) answersPerGroup
        part2 = sum $ map (length . foldl1 intersection . map (fromList @(HashSet Char) . unpack)) answersPerGroup
        answersPerGroup = map (splitOn "\n") . splitOn "\n\n" $ inputText

-- >>> runSolution solution (TestInput "06")
-- (11,6)

-- >>> runSolution solution (RealInput "06")
-- (6680,3117)
