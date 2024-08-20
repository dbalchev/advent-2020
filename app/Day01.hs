module Day01 where
import           AocPrelude
import           Control.Monad (guard)
import           Data.Either   (partitionEithers)
import           Data.Maybe    (maybeToList)
import           Prelude       hiding (lines)

solution inputText = x * y
    where
        ([], numberAndRest) = partitionEithers $ decimal <$> lines inputText
        inputNumbers = fst <$> numberAndRest
        numberToIndex :: HashMap Int Int
        numberToIndex = fromList (zip inputNumbers [0..])

        (x, y) : _ = do
            (xCandidate, xPos) <- zip inputNumbers [0..]
            -- return (xCandidate, xPos)
            let yCandidate = 2020 - xCandidate
            yPos <- maybeToList (numberToIndex !? yCandidate)
            guard $ xPos < yPos
            return (xCandidate, yCandidate)


-- >>> runSolution solution (TestInput "01")
-- 514579

-- >>> runSolution solution (RealInput "01")
-- 744475
