{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Day16 where
import           AocPrelude
import           Data.Either   (rights)
import           Data.Foldable (toList)
import           Prelude       ()

data ParsedInput = ParsedInput {
    validations    :: HashMap Text (Vector (Int, Int)),
    myTicketFields :: Vector Int,
    nearbyTickets  :: Vector (Vector Int)
    } deriving(Show)

parseConstraint constraintStr = (low, high)
    where
        [lowStr, highStr] = splitOn "-" constraintStr
        Right (low, _) = decimal lowStr
        Right (high, _) = decimal highStr

parseValidation validationLine = (fieldName, constraints)
    where
        [fieldName, constraintsStr] = splitOn ": " validationLine
        constraints = fromList @(Vector (Int, Int)) . map parseConstraint . splitOn " or " $ constraintsStr

-- >>> parseValidation "row: 6-11 or 33-44"
-- ("row",RootNode {vecSize = 2, vecShift = 5, vecTail = [(33,44),(6,11)], intVecPtrs = []})

parseInput input = ParsedInput {validations, myTicketFields, nearbyTickets}
    where
        [validationsStr, myTicketStr, nearbyTicketsStr] = splitOn "\n\n" input
        validations = fromList . map parseValidation . lines $ validationsStr
        Just myTicketStrNoPrefix = stripPrefix "your ticket:\n" myTicketStr
        Just nearbyTicketsStrNoPrefix = stripPrefix "nearby tickets:\n" nearbyTicketsStr
        readTickets = fromList . map fst . rights . map decimal . splitOn ","
        myTicketFields = readTickets myTicketStrNoPrefix
        nearbyTickets = fromList . map readTickets . lines $ nearbyTicketsStr

isValidForAny :: HashMap a (Vector (Int, Int)) -> Int -> Bool
isValidForAny validations number = any (`validateField` number) . elems $ validations
    where
        validateField ranges n = any (`valdiateRange` n) ranges
        valdiateRange (low, high) n = low <= n && n <= high

solution input = solution1
    where
        ParsedInput {validations, myTicketFields, nearbyTickets} = parseInput input
        allNearbyTickets = concatMap toList . toList $ nearbyTickets
        solution1 = sum . filter (not . isValidForAny validations) $ allNearbyTickets


-- >>> runSolution solution (TestInput "16")
-- 71

-- >>> runSolution solution (RealInput "16")
-- 20058

