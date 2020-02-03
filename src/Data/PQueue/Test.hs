-- |
-- Module: Data.PQueue.Test
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.PQueue.Test
(
  properties

-- * Indiviual Properties
, prop_empty
, prop_insert
, prop_insert_remove_null
, prop_insert_remove_null_concurrent
, prop_insert_remove_sort
, prop_insert_remove_sorted_concurrent
, prop_insert_remove_concurrent
) where

import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Foldable
import qualified Data.List as L
import Data.Maybe

import Test.QuickCheck
import Test.QuickCheck.Monadic

-- internal modules

import Data.PQueue

-- -------------------------------------------------------------------------- --
-- Properties

properties :: [(String, Property)]
properties =
    [ ("newEmptyPQueue is empty" , property $ prop_empty)
    , ("inserts result in queue of correct size" , property $ prop_insert)
    , ("equal number of inserts and remove result in empty queue" , property $ prop_insert_remove_null)
    , ("equal number of concurrent inserts and remove result in empty queue" , property $ prop_insert_remove_null_concurrent)
    , ("inserting and removeing a list sorts the list", property $ prop_insert_remove_sort)
    , ("concurrently inserting and removing a sorted list yields the original list" , property $ prop_insert_remove_sorted_concurrent)
    , ("concurrently inserting and removing a list yields the items of original list" , property $ prop_insert_remove_concurrent)
    ]

-- -------------------------------------------------------------------------- --
-- Tests

prop_empty :: Property
prop_empty = once $ monadicIO $ do
    q <- run newEmptyPQueue
    x <- run (pQueueIsEmpty q)
    assert x
    s <- run (pQueueSize q)
    assert $ s == 0

prop_insert :: [Int] -> Property
prop_insert l = monadicIO $ do
    s <- run $ do
        q <- newEmptyPQueue
        traverse_ (pQueueInsert q) l
        pQueueSize q
    assert $ s == fromIntegral (length l)

prop_insert_remove_null :: [Int] -> Property
prop_insert_remove_null l = monadicIO $ do
    q <- run newEmptyPQueue
    s <- run $ do
        traverse_ (pQueueInsert q) l
        traverse_ (const $ pQueueRemove q) l
        pQueueSize q
    assert $ s == 0
    assert =<< run (pQueueIsEmpty q)

prop_insert_remove_null_concurrent :: [Int] -> Property
prop_insert_remove_null_concurrent l = monadicIO $ do
    q <- run newEmptyPQueue
    run $ concurrently_
        (traverse_ (pQueueInsert q) l)
        (traverse_ (const $ pQueueRemove q) l)
    s <- run $ pQueueSize q
    assert $ s == 0
    assert =<< run (pQueueIsEmpty q)

prop_insert_remove_sort :: [Int] -> Property
prop_insert_remove_sort l = monadicIO $ do
    q <- run newEmptyPQueue
    l' <- run $ do
        traverse_ (pQueueInsert q) l
        traverse (const $ pQueueRemove q) l
    assert $ L.sort l == l'

prop_insert_remove_sorted_concurrent :: SortedList Int -> Property
prop_insert_remove_sorted_concurrent (Sorted l) = monadicIO $ do
    q <- run newEmptyPQueue
    l' <- run $ snd <$> concurrently
        (traverse_ (pQueueInsert q) l)
        (traverse (const $ pQueueRemove q) l)
    assert $ l == l'

prop_insert_remove_concurrent :: [Int] -> Property
prop_insert_remove_concurrent l = monadicIO $ do
    q <- run newEmptyPQueue
    commands <- pick $ shuffle
        $ (QueueInsert <$> l) ++ (const QueueRemove <$> l)
    l' <- run $ catMaybes
        <$> mapConcurrently (runQueueCommand q) commands
    assert $ L.sort l == L.sort l'

-- -------------------------------------------------------------------------- --
-- Utils

data QueueCommand a = QueueInsert a | QueueRemove
    deriving (Show)

runQueueCommand :: MonadIO m => Ord a => PQueue a -> QueueCommand a -> m (Maybe a)
runQueueCommand q (QueueInsert a) = liftIO (Nothing <$ pQueueInsert q a)
runQueueCommand q QueueRemove = liftIO (Just <$> pQueueRemove q)

