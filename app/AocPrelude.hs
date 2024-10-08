{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module AocPrelude (
    module All,
    module Prelude,
    foo,
    CanBeEmpty(..), FromList(..), ToList(..), ListLike(..), Indexable(..), Splittable(..), HasLength(..), SetLike(..), SetLikeOps(..),
    Insertable(..), Updatable(..), Reversible(..),
    toKeyValuePairs, collectByFirst, counter, trivailSolvePossibleIndices, celuarAutomataMove,
    makeFileName,
    TestInput(..), RealInput(..),
    runSolution
) where


import           Data.HashSet           as All hiding (all, any, append, break,
                                                concat, concatMap, cycle,
                                                delete, difference, drop,
                                                dropWhile, elem, empty, filter,
                                                foldl, foldl', foldl1, foldr,
                                                foldr', foldr1, fromList,
                                                groupBy, head, index, init,
                                                insert, intercalate,
                                                intersection, intersperse,
                                                iterate, last, length, lookup,
                                                map, maximum, member, minimum,
                                                null, partition, repeat,
                                                replicate, reverse, scanl,
                                                scanl1, scanr, scanr1,
                                                singleton, size, snoc, span,
                                                splitAt, tail, take, takeWhile,
                                                toList, transpose, union,
                                                unions, update, zip, zipWith,
                                                (!), (!?))
import           Prelude                hiding (head, last, length, lines, null,
                                         reverse, splitAt, toList, unlines,
                                         unwords, words)

import           Data.HashMap.Lazy      as All hiding (all, any, append, break,
                                                concat, concatMap, cycle,
                                                delete, difference, drop,
                                                dropWhile, elem, empty, filter,
                                                foldl, foldl', foldl1, foldr,
                                                foldr', foldr1, fromList,
                                                groupBy, head, index, init,
                                                insert, intercalate,
                                                intersection, intersperse,
                                                iterate, last, length, lookup,
                                                map, maximum, member, minimum,
                                                null, partition, repeat,
                                                replicate, reverse, scanl,
                                                scanl1, scanr, scanr1,
                                                singleton, size, snoc, span,
                                                splitAt, tail, take, takeWhile,
                                                toList, transpose, union,
                                                unions, update, zip, zipWith,
                                                (!), (!?))
import           Data.Text.Lazy         as All hiding (all, any, append, break,
                                                concat, concatMap, cycle,
                                                delete, difference, drop,
                                                dropWhile, elem, empty, filter,
                                                foldl, foldl', foldl1, foldr,
                                                foldr', foldr1, fromList,
                                                groupBy, head, index, init,
                                                insert, intercalate,
                                                intersection, intersperse,
                                                iterate, last, length, lookup,
                                                map, maximum, member, minimum,
                                                null, partition, repeat,
                                                replicate, reverse, scanl,
                                                scanl1, scanr, scanr1,
                                                singleton, size, snoc, span,
                                                splitAt, tail, take, takeWhile,
                                                toList, transpose, union,
                                                unions, update, zip, zipWith,
                                                (!), (!?))
import           Data.Text.Lazy.Read    as All
import           Data.Vector.Persistent as All hiding (all, any, append, break,
                                                concat, concatMap, cycle,
                                                delete, difference, drop,
                                                dropWhile, elem, empty, filter,
                                                foldl, foldl', foldl1, foldr,
                                                foldr', foldr1, fromList,
                                                groupBy, head, index, init,
                                                insert, intercalate,
                                                intersection, intersperse,
                                                iterate, last, length, lookup,
                                                map, maximum, member, minimum,
                                                null, partition, repeat,
                                                replicate, reverse, scanl,
                                                scanl1, scanr, scanr1,
                                                singleton, size, snoc, span,
                                                splitAt, tail, take, takeWhile,
                                                toList, transpose, union,
                                                unions, update, zip, zipWith,
                                                (!), (!?))

import qualified Data.Foldable
import qualified Data.HashMap.Lazy
import qualified Data.HashSet
import qualified Data.List
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Data.Vector.Persistent
import qualified Prelude

import           Data.Bifunctor         (Bifunctor (bimap, second))
import           Data.Hashable          (Hashable (..))
import           Data.Kind
import           Data.List              (partition)
import           GHC.IO                 (catch)
import           GHC.IO.Exception       (IOException)

foo :: Int -> Int
foo x = 2 * x

class CanBeEmpty a where
    empty :: a
    null :: a -> Bool


instance CanBeEmpty [a] where
    empty = []
    null = Prelude.null

instance CanBeEmpty Text where
    empty = Data.Text.Lazy.empty
    null = Data.Text.Lazy.null

instance CanBeEmpty (HashSet v) where
    empty = Data.HashSet.empty
    null = Data.HashSet.null

instance CanBeEmpty (HashMap k v) where
    empty = Data.HashMap.Lazy.empty
    null = Data.HashMap.Lazy.null

instance CanBeEmpty (Vector a) where
    empty = Data.Vector.Persistent.empty
    null = Data.Vector.Persistent.null

class FromList a where
    type FromElement a
    fromList :: [FromElement a] -> a

instance FromList Text where
    type FromElement Text = Char
    fromList = Data.Text.Lazy.pack

instance (Eq v, Hashable v) => FromList (HashSet v) where
    type (FromElement (HashSet v)) = v
    fromList = Data.HashSet.fromList

instance (Eq k, Hashable k) => FromList (HashMap k v) where
    type (FromElement (HashMap k v)) = (k, v)
    fromList = Data.HashMap.Lazy.fromList

instance FromList (Vector a) where
    type (FromElement (Vector a)) = a
    fromList = Data.Vector.Persistent.fromList

class ToList a where
    type ToElement a
    toList :: a -> [ToElement a]

instance (Foldable (t :: Type -> Type)) => (ToList (t a)) where
    type ToElement (t a) = a
    toList = Data.Foldable.toList

instance ToList Text where
    type ToElement Text = Char
    toList = toList

class ListLike a where
    type Element a
    head :: a -> Element a
    last :: a -> Element a
    singleton :: Element a -> a
    snoc :: a -> Element a -> a

instance ListLike [a] where
    type Element [a] = a
    head = Prelude.head
    last = Prelude.last
    singleton x = [x]
    snoc xs x = xs ++ [x]

instance ListLike Text where
    type Element Text = Char
    head = Data.Text.Lazy.head
    last = Data.Text.Lazy.last
    singleton = Data.Text.Lazy.singleton
    snoc = Data.Text.Lazy.snoc

instance ListLike (Vector a) where
    type Element (Vector a) = a
    head xs = xs ! 0
    last xs = xs ! (n - 1)
        where
            n = length xs
    singleton = Data.Vector.Persistent.singleton
    snoc = Data.Vector.Persistent.snoc

instance (Hashable a) => ListLike (HashSet a) where
    type Element (HashSet a) = a
    head = head . toList
    last = last . toList
    singleton = Data.HashSet.singleton
    snoc = flip insert

class Indexable a where
    type Index a
    type Value a
    (!) :: a -> Index a -> Value a
    (!?) :: a -> Index a -> Maybe (Value a)

instance Indexable Text where
    type Index Text = Int
    type Value Text = Char
    (!) text i = Data.Text.Lazy.index text (fromIntegral i)
    (!?) text i
        | i < fromIntegral (Data.Text.Lazy.length text) = Just (text ! i)
        | otherwise = Nothing

instance Indexable (Vector a) where
    type Index (Vector a) = Int
    type Value (Vector a) = a
    (!) vector i = x
        where
            Just x =vector !? fromIntegral i
    (!?) = Data.Vector.Persistent.index

instance (Eq k, Hashable k) => Indexable (HashMap k v) where
    type Index (HashMap k v) = k
    type Value (HashMap k v) = v
    (!?) = (Data.HashMap.Lazy.!?)
    (!) hashMap key = value
        where
            Just value = hashMap !? key

class (Indexable a) => Updatable a where
    update :: Index a -> Value a -> a -> a

instance Updatable (Vector a) where
    update = Data.Vector.Persistent.update

class Splittable a where
    splitAt :: Int -> a -> (a, a)

instance Splittable Text where
    splitAt i = Data.Text.Lazy.splitAt (fromIntegral i)

instance Splittable [a] where
    splitAt = Data.List.splitAt

class HasLength a where
    length :: a -> Int

instance (Foldable (t :: Type -> Type)) => (HasLength (t a)) where
    length = Prelude.length

instance HasLength Text where
    length = fromIntegral . Data.Text.Lazy.length

class SetLike a where
    type SetItem a
    member :: SetItem a -> a  -> Bool

instance (Eq a) => SetLike [a] where
    type SetItem [a] = a
    member = Data.List.elem

class (SetLike a) => SetLikeOps a where
    intersection :: a -> a -> a
    union :: a -> a -> a
    difference :: a -> a -> a

instance (Hashable a, Eq a) => SetLike (HashSet a) where
    type SetItem (HashSet a) = a
    member = Data.HashSet.member

instance (Hashable a, Eq a) => SetLikeOps (HashSet a) where
    intersection = Data.HashSet.intersection
    union = Data.HashSet.union
    difference = Data.HashSet.difference

instance (Hashable k, Eq k) => SetLike (HashMap k v) where
    type SetItem (HashMap k v) = k
    member = Data.HashMap.Lazy.member

instance (Hashable k, Eq k) => SetLikeOps (HashMap k v) where
    intersection = Data.HashMap.Lazy.intersection
    union = Data.HashMap.Lazy.union
    difference = Data.HashMap.Lazy.difference

class Insertable a where
    type InsertElement a
    type DeleteElement a
    insert :: InsertElement a -> a -> a
    delete :: DeleteElement a -> a -> a

instance (Hashable a, Eq a) => Insertable (HashSet a) where
    type InsertElement (HashSet a) = a
    type DeleteElement (HashSet a) = a
    insert = Data.HashSet.insert
    delete = Data.HashSet.delete

instance (Hashable k, Eq k) => Insertable (HashMap k v) where
    type InsertElement (HashMap k v) = (k, v)
    type DeleteElement (HashMap k v) = k
    insert (key, value) = Data.HashMap.Lazy.insert key value
    delete = Data.HashMap.Lazy.delete

instance {-# OVERLAPPABLE #-} (Hashable a, Foldable (f :: Type -> Type), Eq (f a)) => Hashable (f a) where
    hashWithSalt salt  = hashWithSalt salt . toList

class (Reversible a) where
    reverse :: a -> a

instance (Reversible [a]) where
    reverse = Prelude.reverse

instance (Reversible (Vector a)) where
    reverse = Data.Vector.Persistent.reverse

instance (Reversible Text) where
    reverse = Data.Text.Lazy.reverse

toKeyValuePairs = Data.HashMap.Lazy.toList


counter :: (Hashable a, Eq a) => [a] -> HashMap a Int
counter = foldl (flip (alter (Just . maybe 1 (+1)))) empty

-- | counter test
-- >>> counter "abcabbcd"
-- fromList [('a',2),('b',3),('c',2),('d',1)]

collectByFirst :: (Hashable k, Eq k, ListLike collected, Traversable t) => t (k, Element collected) -> HashMap k collected
collectByFirst = foldl updateMap empty
    where
        updateMap oldMap (newKey, newValue) = alter (Just . maybe (singleton newValue) (`snoc` newValue)) newKey oldMap

trivailSolvePossibleIndices :: (Hashable a, Hashable b) => HashMap a (HashSet b) -> HashMap a (HashSet b)
trivailSolvePossibleIndices = fromList . go . toKeyValuePairs
    where
        hasSingleElementMapping = any ((== 1) . length)
        go mapping
            | not $ hasSingleElementMapping mapping = mapping
            | otherwise = (singleElementKey, fromList [singleElementValue]): go cleanedMap
            where
                (singleElementKey, singleElementValueSet):_ = filter ((==1) . length . snd) mapping
                [singleElementValue] = toList singleElementValueSet
                cleanedMap = map (second (delete singleElementValue)) . filter ((/= singleElementKey) . fst) $ mapping

celuarAutomataMove :: (Hashable a) => (a -> [a]) -> (Bool -> [Bool] -> Bool) -> (HashSet a, HashSet a) -> (HashSet a, HashSet a)
celuarAutomataMove adjElements transitionFunction (oldActiveSet, oldChangedSet) = (newActive, newChanged)
    where
        informedNewState point = transitionFunction (point `member` oldActiveSet) (map (`member` oldActiveSet) (adjElements point))
        (changedToActivated, changedToInactivated) =  bimap (fromList @(HashSet _)) (fromList @(HashSet _)) . partition informedNewState . toList $ oldChangedSet
        newActive = (oldActiveSet `difference` changedToInactivated) `union` changedToActivated
        newActivated = changedToActivated `difference` oldActiveSet
        newDeactivated = changedToInactivated `intersection` oldActiveSet
        newChanged = fromList . concatMap (concatMap adjElements . toList) $ [newActivated, newDeactivated]

-- AoC specific stuff

class AoCInput a where
    readInput :: a -> IO Text

instance AoCInput Text where
    readInput = return

instance AoCInput [Char] where
    readInput = return . Data.Text.Lazy.pack

newtype TestInput = TestInput String
newtype RealInput = RealInput String

makeFileName dirName day = "inputs/" ++ dirName ++ "/" ++ day ++ ".txt"


readAocInput dirName day = do
    let filename = makeFileName dirName day
    catch (Data.Text.Lazy.IO.readFile filename) (\(ex :: IOException)-> Data.Text.Lazy.IO.readFile ("../" ++ filename))

instance AoCInput TestInput where
    readInput (TestInput day) = readAocInput "test" day


instance AoCInput RealInput where
    readInput (RealInput day) = readAocInput "real" day


-- runSolution :: (AoCInput input) => (Text -> a) -> input -> IO a
runSolution solution aocInput = do
    inputValue <- readInput aocInput
    return $ solution inputValue
