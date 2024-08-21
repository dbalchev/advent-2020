{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module AocPrelude (
    module All, module Prelude, foo, CanBeEmpty(..), FromList(..), ListLike(..), Indexable(..), makeFileName, TestInput(..), RealInput(..), runSolution
) where


import           Data.HashSet           as All hiding (all, any, append, break,
                                                concat, concatMap, cycle,
                                                delete, difference, drop,
                                                dropWhile, elem, empty, filter,
                                                foldl, foldl', foldl1, foldr,
                                                foldr', foldr1, fromList, head,
                                                index, init, insert,
                                                intersection, iterate, last,
                                                length, lookup, map, maximum,
                                                member, minimum, null,
                                                partition, repeat, replicate,
                                                reverse, scanl, scanl1, scanr,
                                                scanr1, singleton, size, snoc,
                                                span, splitAt, tail, take,
                                                takeWhile, toList, union,
                                                unions, update, zip, zipWith,
                                                (!))
import           Prelude                hiding (head, lines, null, unlines,
                                         unwords, words)

import           Data.HashMap.Lazy      as All hiding (all, any, append, break,
                                                concat, concatMap, cycle,
                                                delete, difference, drop,
                                                dropWhile, elem, empty, filter,
                                                foldl, foldl', foldl1, foldr,
                                                foldr', foldr1, fromList, head,
                                                index, init, insert,
                                                intersection, iterate, last,
                                                length, lookup, map, maximum,
                                                member, minimum, null,
                                                partition, repeat, replicate,
                                                reverse, scanl, scanl1, scanr,
                                                scanr1, singleton, size, snoc,
                                                span, splitAt, tail, take,
                                                takeWhile, toList, union,
                                                unions, update, zip, zipWith,
                                                (!))
import           Data.Text.Lazy         as All hiding (all, any, append, break,
                                                concat, concatMap, cycle,
                                                delete, difference, drop,
                                                dropWhile, elem, empty, filter,
                                                foldl, foldl', foldl1, foldr,
                                                foldr', foldr1, fromList, head,
                                                index, init, insert,
                                                intersection, iterate, last,
                                                length, lookup, map, maximum,
                                                member, minimum, null,
                                                partition, repeat, replicate,
                                                reverse, scanl, scanl1, scanr,
                                                scanr1, singleton, size, snoc,
                                                span, splitAt, tail, take,
                                                takeWhile, toList, union,
                                                unions, update, zip, zipWith,
                                                (!))
import           Data.Text.Lazy.Read    as All
import           Data.Vector.Persistent as All hiding (all, any, append, break,
                                                concat, concatMap, cycle,
                                                delete, difference, drop,
                                                dropWhile, elem, empty, filter,
                                                foldl, foldl', foldl1, foldr,
                                                foldr', foldr1, fromList, head,
                                                index, init, insert,
                                                intersection, iterate, last,
                                                length, lookup, map, maximum,
                                                member, minimum, null,
                                                partition, repeat, replicate,
                                                reverse, scanl, scanl1, scanr,
                                                scanr1, singleton, size, snoc,
                                                span, splitAt, tail, take,
                                                takeWhile, toList, union,
                                                unions, update, zip, zipWith,
                                                (!))

import qualified Data.HashMap.Lazy
import qualified Data.HashSet
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Data.Vector.Persistent
import qualified Prelude

import           Data.Hashable          (Hashable)

foo :: Int -> Int
foo x = 2 * x

class CanBeEmpty a where
    empty :: a
    null :: a -> Bool

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

class ListLike a where
    type Element a
    head :: a -> Element a
    singleton :: Element a -> a

instance ListLike [a] where
    type Element [a] = a
    head = Prelude.head
    singleton x = [x]

instance ListLike Text where
    type Element Text = Char
    head = Data.Text.Lazy.head
    singleton = Data.Text.Lazy.singleton

class Indexable a where
    type Index a
    type Value a
    (!) :: a -> Index a -> Value a

instance Indexable Text where
    type Index Text = Int
    type Value Text = Char
    (!) text i = Data.Text.Lazy.index text (fromIntegral i)

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
    Data.Text.Lazy.IO.readFile filename

instance AoCInput TestInput where
    readInput (TestInput day) = readAocInput "test" day


instance AoCInput RealInput where
    readInput (RealInput day) = readAocInput "real" day


-- runSolution :: (AoCInput input) => (Text -> a) -> input -> IO a
runSolution solution aocInput = do
    inputValue <- readInput aocInput
    return $ solution inputValue
