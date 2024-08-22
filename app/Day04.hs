{-# LANGUAGE OverloadedStrings #-}
import           AocPrelude
import           Data.Bool     (bool)
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

isValidPassport passport = requiredFields `isSubsetOf` keysSet passport

solution inputText = solution1
    where
        passportsStr = splitOn "\n\n" inputText
        passports :: Vector (HashMap Text Text)
        passports = (fromList . map parsePassport) passportsStr
        solution1 = (sum . map (bool 0 1 . isValidPassport) . toList) passports

-- >>> runSolution solution (TestInput "04")
-- 2

-- >>> runSolution solution (RealInput "04")
-- 192
