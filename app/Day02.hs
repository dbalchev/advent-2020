{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import           AocPrelude
import           Data.Bool  (bool)
import           Prelude    ()

data PasswordLine = PasswordLine
    {
    firstIndex  :: Int,
    secondIndex :: Int,
    testLetter  :: Char,
    password    :: Text
    } deriving(Show)

parsePassword line = PasswordLine {firstIndex, secondIndex, testLetter=head testLetterStr, password}
    where
        [firstPart, password ]= splitOn ": " line
        [range, testLetterStr] = splitOn " " firstPart
        [firstIndexStr, secondIndexStr] = splitOn "-" range
        Right (firstIndex, _) = decimal firstIndexStr
        Right (secondIndex, _) = decimal secondIndexStr

-- >>> parsePassword "1-3 b: cdefg"
-- PasswordLine {firstIndex = 1, secondIndex = 3, testLetter = 'b', password = "cdefg"}

checkPassword1 PasswordLine {firstIndex, secondIndex, testLetter, password} = firstIndex <= testLetterCount && testLetterCount <= secondIndex
    where
        testLetterCount = fromIntegral $ count (singleton testLetter) password

checkPassword2 PasswordLine {firstIndex, secondIndex, testLetter, password}
    = (password ! (firstIndex - 1) == testLetter) /= (password ! (secondIndex - 1) == testLetter)

countPassing check = sum . map (bool 0 1 . check)

solution inputText = (countPassing checkPassword1 (toList passwords), countPassing checkPassword2 (toList passwords))
    where
        inputLines = lines inputText
        passwords :: Vector PasswordLine
        passwords = fromList $ map parsePassword inputLines

-- | Test Day 02
-- >>> runSolution solution (TestInput "02")
-- (2,1)

--- >>> runSolution solution (RealInput "02")
-- (493,593)
