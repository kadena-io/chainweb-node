{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Chainweb.Test.Pact.DbCacheTest (tests) where

import Chainweb.Pact.Backend.DbCache

import Control.Monad.State.Strict

import Data.Aeson
import Data.ByteString.Lazy (toStrict)

import Database.SQLite3.Direct

import GHC.Compact

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact.DbCacheTest"
    [ testCache ]

defaultChunkSize :: Int
defaultChunkSize = 32768

-- | Create entries with a size the corresponds to the default
-- chunk size of the cache.
--
entry :: MonadIO m => Int -> Char -> m ([String], Int)
entry i c = do
    let e = [replicate (i * defaultChunkSize `div` 256) c]
    s <- liftIO $ compactSize =<< compact e
    return (e, fromIntegral s)

combineEntries :: MonadIO m => Monoid a => a -> a -> m (a, Int)
combineEntries a b = do
    let e = a <> b
    s <- liftIO $ compactSize =<< compact e
    return (e, fromIntegral s)

testCache :: TestTree
testCache = testCase "testCache" $ do

  -- Create Items
  (a, sa) <- entry 1 'a'; -- large
  (b0, sb0) <- combineEntries a a -- larger
  (b1, sb1) <- entry 0 'b' -- small
  (c, sc) <- combineEntries b0 b0 -- even larger
  (d, sd) <- entry 0 'd' -- small

  -- cache size (enough to hold a + b0 + b1 + c)
  let cs = DbCacheLimitBytes . fromIntegral $ sa + sb0 + sb1 + sc + 1

  void $ (`runStateT` (emptyDbCache cs :: DbCache [String])) $ do

    -- a: simple insert
    doCheck "a insert @1 -> [a@1]" "a" a 1
    assertEqual' "size a" sa cacheSize
    assertEqual' "count 1" 1 cacheCount

    -- b0: simple insert
    doCheck "b->v0 insert @2 -> [a@1,b0@2]" "b" b0 2
    assertEqual' "size a + b0" (sa + sb0) cacheSize
    assertEqual' "count 2" 2 cacheCount

    -- b1: insert with different values, same key+txid
    doCheck "b->v1 insert @2 -> [a@1,b0@2,b1@2]" "b" b1 2
    assertEqual' "size a + b0 + b1" (sa + sb0 + sb1) cacheSize
    assertEqual' "count 3" 3 cacheCount

    -- c: big insert
    doCheck "c insert @3 -> [a@1,b0@2,b1@2,c@3]" "c" c 3
    assertEqual' "size a + b0 + b1 + c" (sa + sb0 + sb1 + sc) cacheSize
    assertEqual' "count 4" 4 cacheCount

    -- d: small insert to trip limit, evict
    doCheck "d insert @4, evict a@1 -> [b0@2,b1@2,c@3,d@4]" "d" d 4
    assertEqual' "size b0 + b1 + c + d" (sb0 + sb1 + sc + sd) cacheSize
    assertEqual' "count 4" 4 cacheCount

    -- hit b->v0 to avoid eviction, cache stats unchanged
    doCheck "b->v0 hit @5 -> [b1@2,c@3,d@4,b0@5]" "b" b0 5
    assertEqual' "size unchanged" (sb1 + sc + sd + sb0) cacheSize
    assertEqual' "count unchanged" 4 cacheCount

    -- reinsert a to evict b->v1, c
    doCheck "a reinsert, evict b->v1@2 -> [c@3,d@4,b0@5,a@6]" "a" a 6
    assertEqual' "size c + d + b0 + a) - a - b" (sc + sd + sb0 + sa) cacheSize
    assertEqual' "count 4" 4 cacheCount

  where

    doCheck msg k v txid = do
      mc <- StateT (checkDbCache (Utf8 k) (toStrict (encode v)) txid)
      liftIO $ assertEqual msg (Just v) mc

    assertEqual' msg ex act = get >>= liftIO . assertEqual msg ex . act
