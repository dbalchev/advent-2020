{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Day22 where
import           AocPrelude
import           Data.Function (fix)
import           Data.List     (sort)
import           Prelude       ()

readPlayerCards prefix = fmap (fromList @(Vector _). map (read . unpack) . lines) <$> stripPrefix prefix

readInput :: Text -> (Vector Int, Vector Int)
readInput input = (player1, player2)
    where
        [
            readPlayerCards "Player 1:\n" -> Just player1,
            readPlayerCards "Player 2:\n" -> Just player2
            ] = splitOn "\n\n" input

newtype Deque a = Deque ([a], [a]) deriving (Show, Eq)

instance CanBeEmpty (Deque a) where
  empty = Deque (empty, empty)
  null (Deque (front, back)) = null back && null front

instance FromList (Deque a) where
    type FromElement (Deque a) = a
    fromList xs = Deque (xs, empty)

instance Foldable Deque where
  foldMap f (Deque (front, back)) = foldMap f (front ++ reverse back)


popFront (Deque (front, back))
    | null front = popFront (Deque (reverse back, empty))
    | otherwise = (x, Deque (xs, back))
    where
        (x:xs) = front

-- >>> popFront . fromList $ [1, 2, 3]
-- (1,Deque ([2,3],[]))


pushBack (Deque (front, back)) x = Deque (front, x: back)

-- >>> toList $ pushBack (fromList [1, 2, 3]) 4
-- [1,2,3,4]

initialState (player1, player2) = (fromVector player1, fromVector player2)
    where
        fromVector = fromList . toList

updateState old@(player1, player2)
    | null player1 || null player2 = old
    | c1 > c2 = (updateWinner r1, r2)
    | otherwise = (r1, updateWinner r2)
    where
        (c1, r1) = popFront player1
        (c2, r2) = popFront player2
        [s2, s1] = sort [c1, c2]
        updateWinner = (`pushBack` s2) . (`pushBack` s1)

prettyPrint :: (Deque a, Deque b) -> ([a], [b])
prettyPrint (d1, d2) = (toList d1, toList d2)

endCardList (a, b)
    | null a && (not . null) b = b
    | null b && (not . null) a = a

solution input = score
    where
        initial = initialState . readInput $ input
        stateList = iterate updateState initial
        (endState, _):_ = dropWhile (uncurry (/=)) $ zip stateList (tail stateList)
        state1 = updateState initial
        endCards = toList . endCardList $  endState
        score = sum $ zipWith (*) [1..] (reverse endCards)

-- | Day 22
-- >>> runSolution solution (TestInput "22")
-- 306

-- >>> runSolution solution (RealInput "22")
-- 31308

