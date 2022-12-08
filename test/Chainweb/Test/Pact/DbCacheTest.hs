{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Chainweb.Test.Pact.DbCacheTest (tests) where

import Chainweb.Pact.Backend.DbCache

import Control.Monad.State.Strict
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Database.SQLite3.Direct

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact.DbCacheTest"
    [ testCache ]

testCache :: TestTree
testCache = testCase "testCache" $
  void $ (`runStateT` (emptyDbCache 100 :: DbCache [String])) $ do

    let aValue = ["1234567890"]

    -- a: simple insert
    doCheck "a insert @ 1" "a" aValue 1

    assertEqual' "size 14" 14 cacheSize
    assertEqual' "count" 1 cacheCount

    let bValue0 = aValue ++ aValue
        bValue1 = []

    -- b: insert with different values, same key+txid
    doCheck "b->v0 insert @ 2" "b" bValue0 2

    assertEqual' "size + 27" 41 cacheSize
    assertEqual' "count" 2 cacheCount

    doCheck "b->v1 insert @ 2" "b" bValue1 2

    assertEqual' "size + 2" 43 cacheSize
    assertEqual' "count" 3 cacheCount

    -- c: big insert
    let cValue = bValue0 ++ bValue0

    doCheck "c insert @ 3" "c" cValue 3

    assertEqual' "size + 53" 96 cacheSize
    assertEqual' "count" 4 cacheCount

    -- d: small insert to trip limit, evict
    doCheck "d insert @ 4, evict a@1" "d" ["1234"] 4

    assertEqual' "size + 8 - 14" 90 cacheSize
    assertEqual' "count + 1 - 1" 4 cacheCount

    -- hit b->v0 to avoid eviction, cache stats unchanged
    doCheck "b->v0 hit @ 5" "b" bValue0 5

    assertEqual' "size unchanged" 90 cacheSize
    assertEqual' "count unchanged" 4 cacheCount

    -- reinsert a to evict b->v1, c
    doCheck "a reinsert, evict b->v1@2 + c@3" "a" aValue 6

    assertEqual' "size + 14 - 2 - 53" 49 cacheSize
    assertEqual' "count + 1 - 2" 3 cacheCount

  where

    doCheck msg k v txid =
      state (checkDbCache (Utf8 k) (toStrict (encode v)) txid) >>=
        liftIO . assertEqual msg (Just v)

    assertEqual' msg ex act = get >>= liftIO . assertEqual msg ex . act
