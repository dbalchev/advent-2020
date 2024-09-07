{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Day19 where
import           AocPrelude
import           Prelude    ()

data ParsedGrammarExpr = GChar Char | GCompound (Vector (Vector Text)) deriving (Show)

parseGrammarExpr (unpack -> ['"', char, '"']) = GChar char
parseGrammarExpr input = GCompound . fromList . map (fromList . splitOn " ") . splitOn " | " $ input


parseGrammarExprLine (splitOn ": " -> [name, grammarText]) = (name, parseGrammarExpr grammarText)

type Grammar = (HashMap Text ParsedGrammarExpr)
data MatchingGrammar = MChar Char | MOr [MatchingGrammar] | MCat [MatchingGrammar]


matchExpr (MChar chr) (uncons -> Just (firstChar, rest))
    | chr == firstChar = [rest]
    | otherwise        = []

matchExpr (MOr variants) input = do
    variant <- variants
    matchExpr variant input

matchExpr (MCat []) input = [input]
matchExpr (MCat (x:xs)) input = do
    xMatch <- matchExpr x input
    matchExpr (MCat xs) xMatch

matchExpr _ "" = []

convertParsed :: Grammar -> Text -> MatchingGrammar
convertParsed grammar ruleName = compiled
    where
        expr = grammar ! ruleName
        self = convertParsed grammar
        compiled = case expr of
            GChar chr   -> MChar chr
            GCompound v -> MOr . map (MCat . map self . toList) . toList $ v

matchesGrammar grammar startingRule = ("" `member`) . matchExpr (convertParsed grammar startingRule)

countMatching grammar = length . filter (matchesGrammar grammar "0") . lines

solution input = (solution1, solution2)
    where
        [grammarText, testTexts] = splitOn "\n\n" input
        grammar = fromList @Grammar . map parseGrammarExprLine . lines $ grammarText
        solution1 = countMatching grammar testTexts
        updatedLines = [
            "8: 42 | 42 8",
            "11: 42 31 | 42 11 31"
            ]
        updatedGrammar = fromList @Grammar $ (toKeyValuePairs grammar ++ map parseGrammarExprLine updatedLines)
        solution2 = countMatching updatedGrammar testTexts

-- | Test 18 2
-- >>> runSolution solution (TestInput "19")
-- (2,2)

-- >>> runSolution solution (RealInput "19")
-- (136,256)
