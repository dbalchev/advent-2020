module Day25 where
import           AocPrelude
import           Prelude    ()

m = 20201227

generatedSequence gen = zip [0..] (iterate ((`mod` m) . (* gen)) 1)

dualBruteForce :: Integer -> [Integer] -> (Int, Integer)
dualBruteForce gen pks = head . filter ((`member` pks) . snd) $ generatedSequence gen

-- >>> dualBruteForce 7 [5764801, 17807724]
-- (8,5764801)

solve pks = sharedKey
    where
        (i, pk) = dualBruteForce 7 pks
        (otherKey:_) = filter (/= pk) pks
        (_, sharedKey) = generatedSequence otherKey !! i

-- | Day 25
-- >>> solve [5764801, 17807724]
-- 14897079

-- >>> solve [1526110, 20175123]
-- 10924063
