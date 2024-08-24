{-# LANGUAGE OverloadedStrings #-}
module Day05 where
import           AocPrelude
import           Data.Bool  (bool)
import           Prelude    ()
parseBinary one text = foldr addDigit 0 (reverse isOnes)
    where
        isOnes = map (==one) $ unpack text
        addDigit isDigitOne acc = 2 * acc + bool 0 1 isDigitOne


-- >>> parseBinary 'B' "FBFBBFF"
-- 44

-- >>> parseBinary 'R' "RLR"
-- 5


parseZone text = (parseBinary 'B' rowStr, parseBinary 'R' seatStr)
    where
        (rowStr, seatStr) = splitAt 7 text


-- >>> parseZone "FBFBBFFRLR"
-- (44,5)

seatId (row, seat) = 8 * row + seat

-- >>> seatId (44, 5)
-- 357

solution text = maximum seatIds
    where
        seatIds = map (seatId . parseZone) $ lines text

-- >>> runSolution solution (TestInput "05")
-- 820

-- >>> runSolution solution (RealInput "05")
-- 894
