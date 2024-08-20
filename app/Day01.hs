module Day01 where
import           AocPrelude
import           Control.Monad (guard)
import           Data.Either   (partitionEithers)
import           Data.Maybe    (maybeToList)
import           Prelude       hiding (lines)


generalSolution inputNumbers = product . head . go 0 2020
    where
        numberToIndex :: HashMap Int Int
        numberToIndex = fromList (zip inputNumbers [0..])

        go minPos targetSum 1 =  do
            targetPos <- maybeToList (numberToIndex !? targetSum)
            guard $ minPos < targetPos
            return [targetSum]

        go minPos targetSum nToGo = do
            (xCandidate, xPos) <- zip (drop minPos inputNumbers) [minPos..]
            xs <- go xPos (targetSum - xCandidate) (nToGo - 1)
            return $ xCandidate : xs



solution inputText = (generalSolution inputNumbers 2, generalSolution inputNumbers 3)
    where
        ([], numberAndRest) = partitionEithers $ decimal <$> lines inputText
        inputNumbers = fst <$> numberAndRest



-- >>> runSolution solution (TestInput "01")
-- (514579,241861950)

-- >>> runSolution solution (RealInput "01")
-- (744475,70276940)
