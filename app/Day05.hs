{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Day05 where
import           AocPrelude
import           Data.Bool     (bool)
import           Data.Foldable (Foldable (toList))
import           Data.List     (sort)
import           Prelude       ()
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

solution text = (maximum seatIds, mySeatId)
    where
        seatIds = fromList @(Vector Int) $ sort . map (seatId . parseZone) $ lines text
        mySeatId = map snd . filter fst $ zipWith (\a b -> (a + 1 /= b, a + 1)) (toList seatIds) (drop 1 $ toList seatIds)

-- >>> runSolution solution (TestInput "05")
-- (820,[120,568])

-- >>> runSolution solution (RealInput "05")
-- (894,[579])
