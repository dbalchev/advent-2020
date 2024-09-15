{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Day22 where
import           AocPrelude
import           Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Foldable  as DF
import           Data.Function  (fix)
import           Data.List      (intercalate, sort)
import           Prelude        ()

readPlayerCards prefix = fmap (fromList @(Vector _). map (read . unpack) . lines) <$> stripPrefix prefix

readInput :: Text -> (Vector Int, Vector Int)
readInput input = (player1, player2)
    where
        [
            readPlayerCards "Player 1:\n" -> Just player1,
            readPlayerCards "Player 2:\n" -> Just player2
            ] = splitOn "\n\n" input

newtype Deque a = Deque (Int, [a], [a]) deriving (Show, Eq)

instance CanBeEmpty (Deque a) where
  empty = Deque (0, empty, empty)
  null (Deque (_, front, back)) = null back && null front

instance FromList (Deque a) where
    type FromElement (Deque a) = a
    fromList xs = seq l $ Deque (l, xs, empty)
        where
            l = length xs

instance Foldable Deque where
  foldMap f (Deque (_, front, back)) = foldMap f (front ++ reverse back)

-- instance {-# OVERLAPPABLE #-} HasLength (Deque a) where
  length (Deque (l, _, _)) = l

popFront (Deque (n, front, back))
    | null front = popFront (Deque (n - 1,reverse back, empty))
    | otherwise = (x, Deque (n - 1, xs, back))
    where
        (x:xs) = front

-- >>> popFront . fromList $ [1, 2, 3]
-- (1,Deque (2,[2,3],[]))


pushBack (Deque (n, front, back)) x = Deque (n + 1, front, x: back)

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

data State2 = State2 {
    player1Cards :: Deque Int,
    player2Cards :: Deque Int,
    playedHands  :: HashSet (Deque Int, Deque Int)
    }



isPlayer1WinningOld State2 {player1Cards, player2Cards, playedHands}
    | (player1Cards, player2Cards) `member` playedHands = (True, playedHands)
    | c1 >= length r1 && c2 >= length r2 = subgameResult
    where
        (c1, r1) = popFront player1Cards
        (c2, r2) = popFront player2Cards
        subgamePlayer1Cards = fromList . take c1 . toList $ r1
        subgamePlayer2Cards = fromList . take c2 . toList $ r2
        subgameResult = isPlayer1WinningOld (State2 {player1Cards=subgamePlayer1Cards, player2Cards=subgamePlayer2Cards, playedHands})

data Action = HandWon Bool | PushGame ([Int], [Int]) | PopGame Bool deriving (Show)


selectAction :: [(HashSet ([Int], [Int]), ([Int], [Int]))] -> Action
selectAction ((playedHands, (p1, p2)):_)
    | (p1, p2) `member` playedHands = PopGame True
    | null p1 = PopGame False
    | null p2 = PopGame True
    | c1 <= length r1 && c2 <= length r2 = PushGame (s1, s2)
    | otherwise = HandWon (c1 > c2)
    where
        (c1:r1) = p1
        (c2:r2) = p2
        s1 = take c1 r1
        s2 = take c2 r2

makeNewHands True (a:as,b:bs)  = (as ++ [a, b], bs)
makeNewHands False (a:as,b:bs) = (as, bs ++ [b, a])

applyAction :: Action -> [(HashSet ([Int], [Int]), ([Int], [Int]))] -> [(HashSet ([Int], [Int]), ([Int], [Int]))]
applyAction _ [] = []
applyAction action oldStack@((playedHands, (p1, p2)):rest)
    | (PopGame p1Won) <- action = applyAction (HandWon p1Won) rest
    | (PushGame newGame) <- action = (empty, newGame):oldStack
    | (HandWon p1Won) <- action = let newHand = makeNewHands p1Won (p1, p2) in (newPlayedHands, newHand):rest
    where
        newPlayedHands = insert (p1, p2) playedHands
scoreCards cards = sum $ zipWith (*) [1..] (reverse cards)
updateState2 state = applyAction (selectAction state) state
solution input = (score, score2)
    where
        initial = initialState . readInput $ input
        initial2 = bimap toList toList initial
        stateList = iterate updateState initial
        (endState, _):_ = dropWhile (uncurry (/=)) $ zip stateList (tail stateList)
        state1 = updateState initial
        endCards = toList . endCardList $  endState
        score = scoreCards endCards
        state2List = iterate updateState2 [(empty, initial2)]
        [(_, endState2)] = last . takeWhile (not . null) $ state2List
        endCards2 = endCardList endState2
        score2 = scoreCards endCards2

-- | Day 22
-- >>> runSolution solution (TestInput "22")
-- (306,291)

-- >>> runSolution solution (RealInput "22")
-- (31308,33647)

