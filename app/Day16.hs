{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Day16 where
import           AocPrelude
import           Control.Monad  (guard)
import           Data.Bifunctor (Bifunctor (second))
import           Data.Either    (rights)
import           Prelude        ()

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
        nearbyTickets = fromList . map readTickets . lines $ nearbyTicketsStrNoPrefix


validateField ranges n = any (`valdiateRange` n) ranges
valdiateRange (low, high) n = low <= n && n <= high

isValidForAny :: HashMap a (Vector (Int, Int)) -> Int -> Bool
isValidForAny validations number = any (`validateField` number) . elems $ validations

computePossibleIndices :: Vector (Vector Int) -> Vector (Int, Int) -> HashSet Int
computePossibleIndices nearbyTickets constraints  = fromList $ do
    index <- [0..(length (nearbyTickets ! 0) - 1)]
    guard $ all (validateField constraints . (! index)) nearbyTickets
    return index


extractFields fieldFilter mapping ticket = do
    (field, indices) <- toKeyValuePairs mapping
    guard $ fieldFilter field
    index <- toList indices
    return $ ticket ! index


solution fieldFilter input = (solution1, solution2)
    where
        ParsedInput {validations, myTicketFields, nearbyTickets} = parseInput input
        allNearbyTickets = concatMap toList . toList $ nearbyTickets
        solution1 = sum . filter (not . isValidForAny validations) $ allNearbyTickets
        validNearbyTickets = fromList @(Vector _ ) . filter (all (isValidForAny validations)) . toList $ nearbyTickets
        validMappings = fromList @(HashMap _ _). map (second (computePossibleIndices validNearbyTickets)) $ toKeyValuePairs validations
        solvedMappings = trivailSolvePossibleIndices validMappings
        solution2 = product $ extractFields fieldFilter solvedMappings myTicketFields


-- | Test Day 16
-- >>> runSolution (solution (const False)) (TestInput "16")
-- (71,1)

-- | Test Day 16 2
-- >>> runSolution (solution (== (fromList "class"))) (TestInput "16.2")
-- (0,12)

-- >>> runSolution (solution ("departure" `isPrefixOf`)) (RealInput "16")
-- (20058,366871907221)

