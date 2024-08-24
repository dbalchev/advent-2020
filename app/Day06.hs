{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Day06 where
import           AocPrelude
import           Data.Foldable (Foldable (toList))
import           Prelude       ()

solve setOp = sum . map (length . foldl1 setOp . map (fromList @(HashSet Char) . unpack) . toList) . toList

solution inputText = (part1, part2)
    where
        part1 = solve union answersPerGroup
        part2 = solve intersection answersPerGroup
        answersPerGroup = fromList @(Vector _) . map (fromList @(Vector _ ) . splitOn "\n") . splitOn "\n\n" $ inputText

-- >>> runSolution solution (TestInput "06")
-- (11,6)

-- >>> runSolution solution (RealInput "06")
-- (6680,3117)
