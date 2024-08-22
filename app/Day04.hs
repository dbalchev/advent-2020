{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day04 where
import           AocPrelude
import           Data.Bool     (bool)
import           Data.Char     (isDigit, isHexDigit)
import           Data.Foldable (Foldable (toList))
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

extractYear :: Text -> HashMap Text Text -> Maybe Int
extractYear field passport  = ((fst <$>) . either (const Nothing) Just . decimal) =<< passport !? field

inRange (lowest, highest) x = lowest <= x && x <= highest

checkYear range = either (const False) (inRange range . fst) . decimal

checkHeight (hgtValue, hgtUnit) = case hgtUnit of
    "cm" -> inRange (150, 193) hgtValue
    "in" -> inRange (59, 76) hgtValue
    _    -> False

checkHairColor hcl = (hcl ! 0) == '#' && length hcl == 7 && (all isHexDigit . (drop 1 . unpack)) hcl

possibleEcl :: HashSet Text
possibleEcl = fromList ["amb",  "blu", "brn", "gry", "grn", "hzl", "oth"]

checkPassportId pid = length pid == 9 && (all isDigit . unpack) pid

checks = [
    ("byr", checkYear (1920, 2002)),
    ("iyr", checkYear (2010, 2020)),
    ("eyr", checkYear (2020, 2030)),
    ("hgt", either (const False) checkHeight . decimal),
    ("hcl", checkHairColor),
    ("ecl", (`member` possibleEcl)),
    ("pid", checkPassportId)
    ]

isValidPassport2 :: HashMap Text Text -> Bool
isValidPassport2 passport = all applyCheck checks
    where
        applyCheck (key, check) = maybe False check (passport !? key)

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
