{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Day24 where
import           AocPrelude
import           Data.Bool  (bool)
import           Prelude    ()

breakDownList []             = []
breakDownList ('s':'e':rest) = "se":breakDownList rest
breakDownList ('n':'e':rest) = "ne":breakDownList rest
breakDownList ('s':'w':rest) = "sw":breakDownList rest
breakDownList ('n':'w':rest) = "nw":breakDownList rest
breakDownList ('e':rest)     = "e":breakDownList rest
breakDownList ('w':rest)     = "w":breakDownList rest

breakDown = map pack . breakDownList . unpack

-- >>> breakDown . pack $ "esenee"
-- ["e","se","ne","e"]

delta "e"  = (1, 0)
delta "w"  = (-1, 0)
delta "ne" = (0, 1)
delta "sw" = (0, -1)
delta "nw" = (-1, 1)
delta "se" = (1, -1)

add (a, b) (c, d) = (a + c, b + d)

normalize = foldl add (0, 0) . map delta . breakDown

computeBlackTiles = map fst . filter ((==1) . (`mod` 2) . snd) . toKeyValuePairs . counter @(Int, Int) . map normalize . lines

adjTiles tile = tile:(add tile . delta <$> ["e", "w", "ne", "sw", "nw", "se"])

tileTransition False = (== 2) . sum . map (bool 0 1) . tail
tileTransition True  = (`member` [1, 2]) . sum . map (bool 0 1) . tail

nextDayMove :: (HashSet (Int, Int), HashSet (Int, Int)) -> (HashSet (Int, Int), HashSet (Int, Int))
nextDayMove = celuarAutomataMove adjTiles tileTransition


solve nSteps2 input = (length initialBlackTiles, length . fst $ states !! nSteps2)
    where
        initialBlackTiles = computeBlackTiles input
        initialState = (fromList initialBlackTiles, fromList . concatMap adjTiles $ initialBlackTiles)
        states = iterate nextDayMove initialState

-- | Day24
-- >>> runSolution (solve 5) (TestInput "24")
-- (10,23)

-- >>> runSolution (solve 100) (TestInput "24")
-- (10,2208)

-- >>> runSolution (solve 100) (RealInput "24")
-- (420,4206)

