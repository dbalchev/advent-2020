{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day04 where
import           AocPrelude
import           Control.Monad (guard)
import           Data.Bool     (bool)
import           Data.Char     (isDigit, isHexDigit)
import           Data.Foldable (Foldable (toList))
import           Data.Maybe    (isJust)
import           Prelude       ()


requiredFields :: HashSet Text
requiredFields = fromList [
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
    ]

parsePassport :: Text -> HashMap Text Text
parsePassport passportStr = fromList $ do
    [key, value] <- splitOn ":" <$> words passportStr
    return (key, value)

isValidPassport1 passport = requiredFields `isSubsetOf` keysSet passport

extractYear :: HashMap Text Text -> Text -> Maybe Int
extractYear passport field = do
    (year, _) <- either (const Nothing) Just . decimal =<< passport !? field
    return year

possibleEcl :: HashSet Text
possibleEcl = fromList ["amb",  "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidPassport2 :: HashMap Text Text -> Bool
isValidPassport2 passport = isJust $ do
    byr <- extractYear passport "byr"
    guard $ 1920 <= byr && byr <= 2002
    iyr <- extractYear passport "iyr"
    guard $ 2010 <= iyr && iyr <= 2020
    eyr <- extractYear passport "eyr"
    guard $ 2020 <= eyr && eyr <= 2030
    hgt :: Text <- passport !? "hgt"
    (hghValue, hgtUnit) <- (either (const Nothing) Just . decimal) hgt
    guard $ case hgtUnit of
        "cm" -> 150 <= hghValue && hghValue <= 193
        "in" -> 59 <= hghValue && hghValue <= 76
        _    -> False
    hcl <- passport !? "hcl"
    guard $ (hcl ! 0) == '#' && length hcl == 7 && (all isHexDigit . (drop 1 . unpack)) hcl
    ecl <- passport !? "ecl"
    guard $ member ecl possibleEcl
    pid <- passport !? "pid"
    guard $ length pid == 9 && (all isDigit . unpack) pid

    return True


solution inputText = (solution1, solution2)
    where
        passportsStr = splitOn "\n\n" inputText
        passports :: Vector (HashMap Text Text)
        passports = (fromList . map parsePassport) passportsStr
        solution1 = (sum . map (bool 0 1 . isValidPassport1) . toList) passports
        solution2 = (sum . map (bool 0 1 . isValidPassport2) . toList) passports

-- >>> runSolution solution (TestInput "04")
-- (2,2)

-- >>> runSolution solution (TestInput "04.invalid")
-- (4,0)

-- >>> runSolution solution (TestInput "04.valid")
-- (4,4)

-- >>> runSolution solution (RealInput "04")
-- (192,101)
