{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Day24 where
import           AocPrelude
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

solve1 = length . filter ((==1) . (`mod` 2)) . elems . counter @(Int, Int) . map normalize . lines

-- | Day24
-- >>> runSolution solve1 (TestInput "24")
-- 10

-- >>> runSolution solve1 (RealInput "24")
-- 420

