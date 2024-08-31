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

extendedGcd a b = go (a, 1, 0) (b, 0, 1)
    where
        go result (0, _, _)                 = result
        go (r1, s1, t1) oldNew@(r2, s2, t2) = go oldNew (r1 - q * r2, s1 - q * s2, t1 - q * t2)
            where
                q = r1 `div` r2

-- >>> extendedGcd 240 46
-- (2,-9,47)



chineseRemainderTheorem (mod1, rem1) (mod2, rem2) = if modGcd == 1 then result else error "modGcd is not 1"
    where
        (modGcd, q1, q2) = extendedGcd mod1 mod2
        modNew = mod1 * mod2
        remNew = (mod2 * q2 * rem1 + mod1 * q1 * rem2) `mod` modNew
        result = (modNew, remNew)

-- >>> chineseRemainderTheorem (5, 2) (7, 4)
-- 32


solve2 :: (Integral a, Foldable t) => t (Maybe a) -> Integer
solve2 buses = rem2
    where
        (mod2, rem2) = foldl chineseRemainderTheorem (1, 0) $ do
            (i, Just bus) <- zip [0..] (toList buses)
            return (fromIntegral bus, fromIntegral (bus - i))

-- >>> solve2 [Just 17, Nothing, Just 13, Just 19]
-- 3417

solution input = (wait1 * bus1, solve2 buses)
    where
        timestampLine:busesLine:_ = lines input
        Right (timestamp, _) = decimal timestampLine
        buses = fromList @(Vector (Maybe Int)) . map readBus . splitOn "," $ busesLine
        (wait1, bus1) = solve1 timestamp buses



-- >>> runSolution solution (TestInput "13")
-- (295,1068781)

-- >>> runSolution solution (RealInput "13")
-- (3966,800177252346225)

