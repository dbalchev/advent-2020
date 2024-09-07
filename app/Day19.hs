{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Day19 where
import           AocPrelude
import           Data.Foldable (Foldable (toList))
import           Prelude       ()

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

convertParsed :: Grammar -> Text -> MatchingGrammar
convertParsed grammar ruleName = compiled
    where
        expr = grammar ! ruleName
        self = convertParsed grammar
        compiled = case expr of
            GChar chr   -> MChar chr
            GCompound v -> MOr . map (MCat . map self . toList) . toList $ v

matchesGrammar grammar startingRule = ("" `member`) . matchExpr (convertParsed grammar startingRule)

solution input = solution1
    where
        [grammarText, testTexts] = splitOn "\n\n" input
        grammar = fromList @Grammar . map parseGrammarExprLine . lines $ grammarText
        solution1 = length . filter (matchesGrammar grammar "0") . lines $ testTexts

-- >>> runSolution solution (TestInput "19")
-- 2

-- >>> runSolution solution (RealInput "19")
-- 136
