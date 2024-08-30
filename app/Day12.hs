{-# LANGUAGE TypeApplications #-}
module Day12 where
import           AocPrelude
import           Prelude    ()
parseLine line = (instructionType, arg)
    where
        Just (instructionType, argStr) = uncons line
        Right (arg, _) = decimal argStr

updateState1 (position@(posN, posE), direction@(dirN, dirE)) (instructionType, arg)
    | instructionType == 'N' = ((posN + arg, posE), direction)
    | instructionType == 'S' = ((posN - arg, posE), direction)
    | instructionType == 'E' = ((posN, posE + arg), direction)
    | instructionType == 'W' = ((posN, posE - arg), direction)
    | instructionType == 'F' = ((posN + dirN * arg, posE + dirE * arg), direction)
    | instructionType `member` fromList @(HashSet _) ['L', 'R'] && arg == 180 = (position, (-dirN, -dirE))
    | (instructionType, arg) `member` fromList @(HashSet _) [('L', 90), ('R', 270)]  = (position, (dirE, -dirN))
    | (instructionType, arg) `member` fromList @(HashSet _) [('L', 270), ('R', 90)]  = (position, (-dirE, dirN))


updateState2 (position@(posN, posE), direction@(dirN, dirE)) (instructionType, arg)
    | instructionType == 'N' = (position, (dirN + arg, dirE))
    | instructionType == 'S' = (position, (dirN - arg, dirE))
    | instructionType == 'E' = (position, (dirN, dirE + arg))
    | instructionType == 'W' = (position, (dirN, dirE - arg))
    | instructionType == 'F' = ((posN + dirN * arg, posE + dirE * arg), direction)
    | instructionType `member` fromList @(HashSet _) ['L', 'R'] && arg == 180 = (position, (-dirN, -dirE))
    | (instructionType, arg) `member` fromList @(HashSet _) [('L', 90), ('R', 270)]  = (position, (dirE, -dirN))
    | (instructionType, arg) `member` fromList @(HashSet _) [('L', 270), ('R', 90)]  = (position, (-dirE, dirN))

dist (x, y) = abs x + abs y

solution input = (dist lastPos1, dist lastPos2)
    where
        instructions = fromList @(Vector _) . map parseLine . lines $ input
        (lastPos1, _) = foldl updateState1 ((0, 0), (0, 1)) instructions
        (lastPos2, _) = foldl updateState2 ((0, 0), (1, 10)) instructions


-- >>> runSolution solution (TestInput "12")
-- (25,286)

-- >>> runSolution solution (RealInput "12")
-- (858,39140)
