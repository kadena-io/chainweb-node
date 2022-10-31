{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Mempool.TxFilter
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Mempool.TxFilter
( TxFilterRule(..)
, txFilterRuleSelector
, txFilterRuleRegex
, txFilterRuleExpiration
, txFilterRuleName
, checkRules
) where

import Control.Lens.TH

import Data.Aeson
import qualified Data.Aeson.Types as A
import Data.Bifunctor
import Data.Function
import qualified Data.Text as T
import Data.Time

import GHC.Generics (Generic)

import qualified Text.Regex.TDFA as R
import qualified Text.Regex.TDFA.Text as R

-- internal modules
import Chainweb.Mempool.Mempool
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Regex

data Regex = Regex
    { _regexLiteral :: !T.Text
    , _regexCompiled :: !R.Regex
    }
    deriving (Generic)

instance Show Regex where
    show = show . _regexLiteral

instance Eq Regex where
    (==) = (==) `on` _regexLiteral
    {-# INLINE (==) #-}

instance Ord Regex where
    compare = compare `on` _regexLiteral
    {-# INLINE compare #-}

instance ToJSON Regex where
    toJSON = toJSON . _regexLiteral
    toEncoding = toEncoding . _regexLiteral
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON Regex where
    parseJSON = withText "Regex" $ \t -> case R.compile R.defaultCompOpt R.defaultExecOpt t of
        Right r -> return $ Regex t r
        Left e -> fail $ "failed to parse regular expression: " <> e
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Selector

-- | The components of a 'Selector' must not contain '.'.
--
newtype Selector = Selector { _getSelector :: [T.Text] }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Selector where
    toJSON = toJSON . selectorText
    toEncoding = toEncoding . selectorText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON Selector where
    parseJSON = withText "Selector" $ return . Selector . T.split (== '.')
    {-# INLINE parseJSON #-}

selectorText :: Selector -> T.Text
selectorText = T.intercalate "." . _getSelector
{-# INLINE selectorText #-}

-- -------------------------------------------------------------------------- --
-- Tx Filter Rule

data TxFilterRule = TxFilterRule
    { _txFilterRuleName :: !T.Text
        -- ^ Should be unique
    , _txFilterRuleSelector :: !Selector
    , _txFilterRuleRegex :: !Regex
    , _txFilterRuleExpiration :: !UTCTime
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''TxFilterRule

txFilterRuleProperties :: KeyValue kv => TxFilterRule -> [kv]
txFilterRuleProperties o =
    [ "name" .= _txFilterRuleName o
    , "selector" .= _txFilterRuleSelector o
    , "regex" .= _txFilterRuleRegex o
    , "expiration" .= _txFilterRuleExpiration o
    ]
{-# INLINE txFilterRuleProperties #-}

instance ToJSON TxFilterRule where
    toJSON = object . txFilterRuleProperties
    toEncoding = pairs . mconcat . txFilterRuleProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON TxFilterRule where
    parseJSON = withObject "TxFilterRule" $ \o -> TxFilterRule
        <$> o .: "name"
        <*> o .: "selector"
        <*> o .: "regex"
        <*> o .: "maxHeight"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Result

data TxFilterRuleResult
    = TxFilterRuleNoMatch
        -- ^ The regular expression does not match, which means the TX is good.
    | TxFilterRuleSelectorNotFound T.Text
        -- ^ The selector did not apply to the TX. The TX is good, but a warning
        -- is returned, because this may indicate a bogus rule.
    | TxFilterRuleExpired
        -- ^ The rule has expired and is not applied, which means that the TX is
        -- good.
    | TxFilterRuleMatch T.Text
        -- ^ Rule matches, which means that the TX is bad.
    deriving (Show, Eq, Ord, Generic)

instance ToJSON TxFilterRuleResult where
  toJSON = resultjson object
  toEncoding = resultjson (pairs . mconcat)
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

resultjson :: KeyValue kv => ([kv] -> b) -> TxFilterRuleResult -> b
resultjson f (TxFilterRuleMatch m) = f
    [ "pass" .= False
    , "match" .= m
    ]
resultjson f TxFilterRuleNoMatch = f
    [ "pass" .= True
    ]
resultjson f (TxFilterRuleSelectorNotFound w) = f
    [ "pass" .= True
    , "warning" .= w
    ]
resultjson f TxFilterRuleExpired = f
    [ "pass" .= True
    , "expired" .= True
    ]

-- | Return whether a 'TxFilterRuleResult' indicates that the TX is good.
--
txFilterRuleIsPass :: TxFilterRuleResult -> Bool
txFilterRuleIsPass TxFilterRuleMatch{} = False
txFilterRuleIsPass _ = True

-- | Create a report for 'TxFilterRuleResult'
--
txFilterRuleReport
    :: Value
        -- ^ JSON value of the Transaction
    -> [(TxFilterRule, TxFilterRuleResult)]
    -> Maybe T.Text
txFilterRuleReport tx l = case filter (noMatch . snd) l of
    [] -> Nothing
    x -> Just $ encodeToText $ object
        [ "tx" .= tx
        , "results" .= format <$> x
        ]
  where
    nonMatches = filter noMatch l
    noMatch TxFilterRuleNoMatch = False
    noMatch _ = True
    format (rule, result) = object
        [ "rule" .= rule
        , "result" .= result
        ]

-- -------------------------------------------------------------------------- --
-- Rule Matching

-- | Transfor transaction to JSON value
--
-- The mempool knows nothing about the code of transactions. Actually, it does
-- not even require that txs include code, as witnessed by MockTx. All semantics
-- are encapsulated in the 'TransactionConfig' function record.
--
-- The 'txCodec' from the 'TransactionConfig' provides a way to serialize a
-- transaction to 'B.ByteString' and we know that Chainweb Transactions can be
-- parsed as JSON.
--
-- The bytestring encoding is cached in the transaction as short bytestring. So,
-- the costs are too memcopy the bytestring and parse it into 'Value.
--
tx2value:: TransactionConfig t -> t -> Either T.Text Value
tx2value txcfg = first T.pack
    . eitherDecodeStrict
    . codecEncode (txCodec txcfg)
{-# INLINE tx2value #-}

checkRules :: TransactionConfig t -> t -> [TxFilterRule] -> Either T.Text [TxFilterRuleResult]
checkRules txcfg t rules = case tx2value txcfg t of
    Left e -> Left $ "failed to parse tx as JSON: " <> T.pack e -- TODO warn
    Right x -> all (checkRule x) rules

-- | Checks whether rule matches. Possible outcomes are:
--
-- * Rule does not match (tx is good):
--   * selector not found -> warn the user.
--   * regular expression does not match
--
-- * Rule does match (tx is bad) -> report the match
--
-- * Parsing of tx fails. -> This is a bug, because txs got submitted in JSON format
--
-- The type is thus: Either ParseFailure (Either Match (Maybe SelectorNotFound))
--
checkRule :: Value -> TxFilterRule -> Either T.Text TxFilterRuleResult
checkRule v r = case A.parseEither select v of
    Left e -> Right $ "No match for selector: " <> T.pack e
    Right x -> checkRegex x

  where
    regex = _regexCompiled $ _txFilterRuleRegex r
    selector = _getSelector $ _txFilterRuleSelector r

    checkRegex x = case R.execute regex x of
        Left e -> error e
        Right Nothing -> Right $ "No match of regex " <> _regexLiteral (_txFilterRuleRegex r)
        Right Just{} -> Left $ "match of regex " <> _regexLiteral (_txFilterRuleRegex r)

    select :: Value -> A.Parser T.Text
    select = go id selector
      where
        go p [] = withText (showSelector p) return
        go p (h:t) = do
            let p' = p . (h:)
            withObject (showSelector p') $ \o ->
                go p' t =<< o .: h
        showSelector x = T.unpack . selectorText . Selector $ x []

