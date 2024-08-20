module AocPrelude (
    module All, foo, CanBeEmpty(..)
) where

import           Data.HashSet           as All hiding (append, delete,
                                                difference, drop, dropWhile,
                                                empty, filter, foldl, foldl',
                                                foldr, foldr', fromList, index,
                                                insert, intersection, length,
                                                map, member, null, partition,
                                                reverse, singleton, size, snoc,
                                                splitAt, take, takeWhile,
                                                toList, union, unions, update)

import           Data.HashMap.Lazy      as All hiding (append, delete,
                                                difference, drop, dropWhile,
                                                empty, filter, foldl, foldl',
                                                foldr, foldr', fromList, index,
                                                insert, intersection, length,
                                                map, member, null, partition,
                                                reverse, singleton, size, snoc,
                                                splitAt, take, takeWhile,
                                                toList, union, unions, update)
import           Data.Text.Lazy         as All hiding (append, delete,
                                                difference, drop, dropWhile,
                                                empty, filter, foldl, foldl',
                                                foldr, foldr', fromList, index,
                                                insert, intersection, length,
                                                map, member, null, partition,
                                                reverse, singleton, size, snoc,
                                                splitAt, take, takeWhile,
                                                toList, union, unions, update)
import           Data.Vector.Persistent as All hiding (append, delete,
                                                difference, drop, dropWhile,
                                                empty, filter, foldl, foldl',
                                                foldr, foldr', fromList, index,
                                                insert, intersection, length,
                                                map, member, null, partition,
                                                reverse, singleton, size, snoc,
                                                splitAt, take, takeWhile,
                                                toList, union, unions, update)

import qualified Data.HashMap.Lazy
import qualified Data.HashSet
import qualified Data.Text.Lazy
import qualified Data.Vector.Persistent

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
