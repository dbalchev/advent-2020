{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Day13 where
import           AocPrelude
import           Data.Either   (rights)
import           Data.Foldable (Foldable (toList))
import           Prelude       ()

readBus "x" = Nothing
readBus numberStr = Just number
    where
        Right (number, _) = decimal numberStr


waitTime timestamp busId = ((timestamp + busId - 1) `div` busId) * busId - timestamp

solve1 timestamp buses = minimum $ do
    Just bus <- toList buses
    return (waitTime timestamp bus, bus)

solution input = wait1 * bus1
    where
        timestampLine:busesLine:_ = lines input
        Right (timestamp, _) = decimal timestampLine
        buses = fromList @(Vector (Maybe Int)) . map readBus . splitOn "," $ busesLine
        (wait1, bus1) = solve1 timestamp buses


-- >>> runSolution solution (TestInput "13")
-- 295

-- >>> runSolution solution (RealInput "13")
-- 3966

