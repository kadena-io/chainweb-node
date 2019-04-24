{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module: Chainweb.Utils.Paging
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Utils.Paging
(
-- * Limit
  Limit(..)

-- * Page
, Page(..)
, pageLimit
, pageItems
, pageNext

-- * Next Item
, NextItem(..)
, _getNextItem
, getNextItem
, isExclusive
, isInclusive
, nextItemToText
, nextItemFromText

-- * End-Of-Stream
, Eos(..)
, isEos
, atEos

-- * Tools for creating pages from streams
, finitePrefixOfInfiniteStreamToPage
, finiteStreamToPage
, seekFiniteStreamToPage

-- * Properties
, properties
) where

import Control.Lens (Getter, to)
import Control.Lens.TH
import Control.Monad.Catch
import Control.Monad.Identity

import Data.Aeson
import Data.Functor.Of
import Data.Hashable
import Data.Maybe
import qualified Data.Text as T

import GHC.Generics (Generic)

import Numeric.Natural

import qualified Streaming.Prelude as S

import Test.QuickCheck
import Test.QuickCheck.Instances ({- Arbitrary Natural -})

-- internal modules

import Chainweb.Utils hiding ((==>))

-- -------------------------------------------------------------------------- --
-- Limit

-- | Limit the result of a query to a maximum number of items
--
newtype Limit = Limit { _getLimit :: Natural }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable)
    deriving newtype (Num, Real, Integral, Enum, Ord)

instance Arbitrary Limit where
  arbitrary = Limit <$> arbitrary

-- -------------------------------------------------------------------------- --
-- Page

data Page k a = Page
    { _pageLimit :: !Limit
        -- ^ The number of items in the page
    , _pageItems :: ![a]
        -- ^ The items of the page
    , _pageNext :: !(Maybe k)
        -- ^ A cursor for querying the next page, if there is any. The value
        -- is given the next parameter of the respective query interface.
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''Page

instance (HasTextRepresentation k, ToJSON k, ToJSON a) => ToJSON (Page k a) where
    toJSON p = object
        [ "limit" .= _getLimit (_pageLimit p)
        , "items" .= _pageItems p
        , "next" .= _pageNext p
        ]

instance (HasTextRepresentation k, FromJSON k, FromJSON a) => FromJSON (Page k a) where
    parseJSON = withObject "page" $ \o -> Page
        <$> (Limit <$> (o .: "limit"))
        <*> o .: "items"
        <*> o .: "next"

-- -------------------------------------------------------------------------- --
-- Next Item

-- | When seeking a position in a stream, define if the given position
-- is inclusive or exclusive.
--
-- Inclusive: return all items of the stream starting with the given key.
-- Exclusive: return all items of the stream starting immidiately after the given key.
--
data NextItem k
    = Inclusive k
    | Exclusive k
    deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable)

_getNextItem :: NextItem k -> k
_getNextItem (Inclusive k) = k
_getNextItem (Exclusive k) = k
{-# INLINE _getNextItem #-}

getNextItem :: Getter (NextItem k) k
getNextItem = to _getNextItem
{-# INLINE getNextItem #-}

isInclusive :: NextItem k -> Bool
isInclusive Inclusive{} = True
isInclusive _ = False

isExclusive :: NextItem k -> Bool
isExclusive Exclusive{} = True
isExclusive _ = False

nextItemToText :: HasTextRepresentation k => NextItem k -> T.Text
nextItemToText (Inclusive k) = "inclusive:" <> toText k
nextItemToText (Exclusive k) = "exclusive:" <> toText k

nextItemFromText :: MonadThrow m => HasTextRepresentation k => T.Text -> m (NextItem k)
nextItemFromText t = case T.break (== ':') t of
    (a, b)
        | a == "inclusive" -> Inclusive <$> fromText (T.drop 1 b)
        | a == "exclusive" -> Exclusive <$> fromText (T.drop 1 b)
        | T.null b -> throwM . TextFormatException $ "missing ':' in next item: \"" <> t <> "\"."
        | otherwise -> throwM $ TextFormatException $ "unrecognized next item: \"" <> t <> "\"."

instance HasTextRepresentation k => HasTextRepresentation (NextItem k) where
    toText = nextItemToText
    {-# INLINE toText #-}
    fromText = nextItemFromText
    {-# INLINE fromText #-}

instance HasTextRepresentation k => ToJSON (NextItem k) where
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance HasTextRepresentation k => FromJSON (NextItem k) where
    parseJSON = parseJsonFromText "NextItem"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- End-Of-Stream

-- | Data type to indicate end of stream
--
newtype Eos = Eos { _getEos :: Bool }
    deriving stock (Eq, Show, Ord, Generic)
    deriving newtype (Enum, Bounded, FromJSON, ToJSON)

isEos :: Eos -> Bool
isEos = _getEos

atEos :: Monad m => S.Stream (Of a) m () -> m Eos
atEos = fmap (Eos . isNothing) . S.head_

-- -------------------------------------------------------------------------- --
-- Tools for turning streams into pages

-- | Create page from a non-empty stream that is a non-blocking finite prefix of
-- a possibly blocking infinite stream.
--
-- If the given stream contains more items than requested by the caller an
-- 'Inclusive' cursor is added to the page. Otherwise the last item of the
-- stream is added as 'Exclusive' cursor.
--
-- If the input stream is empty we assume that it is because of a limiting
-- filter that results in a query for a finite stream. No cursor is returned.
--
-- For an empty input we can't return a next cursor. We can't return just
-- 'Nothing' because that is used in a 'Page' to signal the end of the stream,
-- which contradicts the assumption that the input stream is the prefix of an
-- infinite stream. So, when we see an empty stream we assume that it's empty
-- because of some filter and return 'Nothing'
--
finitePrefixOfInfiniteStreamToPage
    :: MonadThrow m
    => (a -> k)
    -> Maybe Limit
    -> S.Stream (Of a) m ()
    -> m (Page (NextItem k) a)
finitePrefixOfInfiniteStreamToPage k limit s = do
    (items' :> limit' :> lastKey :> tailStream) <- S.toList
        . S.length
        . S.copy
        . S.last
        . S.copy
        . maybe (mempty <$) (\n -> S.splitAt (int $ _getLimit n)) limit
        $ s
    maybeNext <- fmap k <$> S.head_ tailStream

    return $ Page (int limit') items' $ case maybeNext of
        Nothing -> case lastKey of
            Nothing -> Nothing
            Just l -> Just (Exclusive $ k l)
        Just next -> Just (Inclusive next)

-- | Create 'Page' from a (possibly empty) prefix of a non-blocking finite
-- stream. If the input stream has more than the requested number of items
-- an 'Inclusive' cursor is added. Otherwise it is assumed that the stream
-- has ended and 'Nothing' is returned as cursor.
--
finiteStreamToPage
    :: Monad m
    => (a -> k)
    -> Maybe Limit
    -> S.Stream (Of a) m ()
    -> m (Page (NextItem k) a)
finiteStreamToPage k limit s = do
    (items' :> limit' :> tailStream) <- S.toList
        . S.length
        . S.copy
        . maybe (mempty <$) (\n -> S.splitAt (int $ _getLimit n)) limit
        $ s
    next <- fmap (Inclusive . k) <$> S.head_ tailStream
    return $ Page (int limit') items' next

-- | Quick and dirty pagin implementation. Usage should be avoided.
--
seekFiniteStreamToPage
    :: Monad m
    => Eq k
    => (a -> k)
    -> Maybe (NextItem k)
    -> Maybe Limit
    -> S.Stream (Of a) m ()
    -> m (Page (NextItem k) a)
seekFiniteStreamToPage k next limit = finiteStreamToPage k limit
    . case next of
        Nothing -> id
        Just (Exclusive n) -> S.drop 1 . S.dropWhile (\x -> k x /= n)
        Just (Inclusive n) -> S.dropWhile (\x -> k x /= n)

-- -------------------------------------------------------------------------- --
-- Properties

prop_streamToPage_limit :: [Int] -> Limit -> Property
prop_streamToPage_limit l i = i <= len l ==> actual === expected
#if MIN_VERSION_QuickCheck(2,12,0)
    & cover 1 (i == len l) "limit == length of stream"
    & cover 1 (i == 0) "limit == 0"
    & cover 1 (length l == 0) "length of stream == 0"
#endif
  where
    s = S.each l
    is = take (int i) l
    actual = runIdentity $ finiteStreamToPage id (Just i) s
    expected = Page i is (Inclusive <$> listToMaybe (drop (int i) l))

prop_streamToPage_id :: [Int] -> Property
prop_streamToPage_id l = actual === expected
#if MIN_VERSION_QuickCheck(2,12,0)
    & cover 1 (length l == 0) "len l == 0"
#endif
  where
    s = S.each l
    actual = runIdentity $ finiteStreamToPage id Nothing s
    expected = Page (len l) l Nothing

properties :: [(String, Property)]
properties =
    [ ("streamToPage_limit", property prop_streamToPage_limit)
    , ("streamToPage_id", property prop_streamToPage_id)
    ]

