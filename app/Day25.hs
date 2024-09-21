module Day25 where
import           AocPrelude
import           Control.Monad (guard)
import           Prelude       ()

m = 20201227

generatedSequence gen = iterate advance (0, 1)
    where
        advance (i, x) = (i + 1, x * gen `mod` m)

bruteForce gen pk = fst . head  . filter ((== pk) . snd) . generatedSequence $ gen

-- >>> bruteForce 7 5764801
-- 8

-- >>> bruteForce 7 17807724
-- 11

dualBruteForce :: Integer -> [Integer] -> (Int, Integer)
dualBruteForce gen pks = head $ do
    (i, x) <- generatedSequence gen
    guard $ x `member` pks
    return (i, x)

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
