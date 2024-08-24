{-# LANGUAGE OverloadedStrings #-}

module Day06 where
import           AocPrelude
import           Data.List  (nub, sort)
import           Prelude    ()

solution inputText = sum $ map (length . nub . sort . concatMap unpack) answersPerGroup
    where
        answersPerGroup = map (splitOn "\n") . splitOn "\n\n" $ inputText

-- >>> runSolution solution (TestInput "06")
-- 11

-- >>> runSolution solution (RealInput "06")
-- 6680
