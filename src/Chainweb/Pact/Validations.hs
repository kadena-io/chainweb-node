{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Chainweb.Pact.Validations
-- Copyright: Copyright © 2018,2019,2020,2021,2022 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Lars Kuhtz, Emily Pillmore, Stuart Popejoy, Greg Hale
-- Stability: experimental
--
-- Validation checks for transaction requests.
-- These functions are meant to be shared between:
--  - The codepath for adding transactions to the mempool
--  - The codepath for letting users test their transaction via /local
--
module Chainweb.Pact.Validations
( -- * Local metadata _validation
  assertLocalMetadata
  -- * Validation checks
, assertParseChainId
, assertChainId
, assertGasPrice
, assertNetworkId
, assertSigSize
, assertTxSize
, IsWebAuthnPrefixLegal(..)
, assertValidateSigs
, assertTxTimeRelativeToParent
, assertTxNotInFuture
, assertCommand
  -- * Defaults
, defaultMaxCommandUserSigListSize
, defaultMaxCoinDecimalPlaces
, defaultMaxTTL
, defaultLenientTimeSlop
) where

import Control.Lens

import Data.Decimal (decimalPlaces)
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Short as SBS
import Data.Word (Word8)

-- internal modules

import Chainweb.BlockHeader (ParentCreationTime(..), BlockHeader(..), ParentHeader(..))
import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.Pact.Types
import Chainweb.Pact.Utils (fromPactChainId)
import Chainweb.Pact.Service.Types
import Chainweb.Time (Seconds(..), Time(..), secondsToTimeSpan, scaleTimeSpan, second, add)
import Chainweb.Transaction (cmdTimeToLive, cmdCreationTime, PayloadWithText, payloadBytes, payloadObj, IsWebAuthnPrefixLegal(..))
import Chainweb.Version
import Chainweb.Version.Guards (isWebAuthnPrefixLegal, validPPKSchemes)

import qualified Pact.Types.Gas as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.ChainId as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.ChainMeta as P
import qualified Pact.Types.KeySet as P
import qualified Pact.Parse as P


-- | Check whether a local Api request has valid metadata
--
assertLocalMetadata
    :: P.Command (P.Payload P.PublicMeta c)
    -> TxContext
    -> Maybe LocalSignatureVerification
    -> PactServiceM logger tbl (Either (NonEmpty Text) ())
assertLocalMetadata cmd@(P.Command pay sigs hsh) txCtx sigVerify = do
    v <- view psVersion
    cid <- view chainId
    bgl <- view psBlockGasLimit

    let bh = ctxCurrentBlockHeight txCtx
    let validSchemes = validPPKSchemes v cid bh
    let webAuthnPrefixLegal = isWebAuthnPrefixLegal v cid bh

    let P.PublicMeta pcid _ gl gp _ _ = P._pMeta pay
        nid = P._pNetworkId pay
        signers = P._pSigners pay

    let errs = catMaybes
          [ eUnless "Unparseable transaction chain id" $ assertParseChainId pcid
          , eUnless "Chain id mismatch" $ assertChainId cid pcid
          , eUnless "Transaction Gas limit exceeds block gas limit" $ assertBlockGasLimit bgl gl
          , eUnless "Gas price decimal precision too high" $ assertGasPrice gp
          , eUnless "Network id mismatch" $ assertNetworkId v nid
          , eUnless "Signature list size too big" $ assertSigSize sigs
          , eUnless "Invalid transaction signatures" $ sigValidate validSchemes webAuthnPrefixLegal signers
          , eUnless "Tx time outside of valid range" $ assertTxTimeRelativeToParent pct cmd
          ]

    pure $ case nonEmpty errs of
      Nothing -> Right ()
      Just vs -> Left vs
  where
    sigValidate validSchemes webAuthnPrefixLegal signers
      | Just NoVerify <- sigVerify = True
      | otherwise = assertValidateSigs validSchemes webAuthnPrefixLegal hsh signers sigs

    pct = ParentCreationTime
      . _blockCreationTime
      . _parentHeader
      . _tcParentHeader
      $ txCtx

    eUnless t assertion
      | assertion = Nothing
      | otherwise = Just t

-- | Check whether a particular Pact chain id is parseable
--
assertParseChainId :: P.ChainId -> Bool
assertParseChainId = isJust . fromPactChainId

-- | Check whether the chain id defined in the metadata of a Pact/Chainweb
-- command payload matches a given chain id.
--
-- The supplied chain id should be derived from the current
-- chainweb node structure
--
assertChainId :: ChainId -> P.ChainId -> Bool
assertChainId cid0 cid1 = chainIdToText cid0 == P._chainId cid1

-- | Check and assert that 'GasPrice' is rounded to at most 12 decimal
-- places.
--
assertGasPrice :: P.GasPrice -> Bool
assertGasPrice (P.GasPrice (P.ParsedDecimal gp)) = decimalPlaces gp <= defaultMaxCoinDecimalPlaces

-- | Check and assert that the 'GasLimit' of a transaction is less than or eqaul to
-- the block gas limit
--
assertBlockGasLimit :: P.GasLimit -> P.GasLimit -> Bool
assertBlockGasLimit bgl tgl = bgl >= tgl

-- | Check and assert that 'ChainwebVersion' is equal to some pact 'NetworkId'.
--
assertNetworkId :: ChainwebVersion -> Maybe P.NetworkId -> Bool
assertNetworkId _ Nothing = False
assertNetworkId v (Just (P.NetworkId nid)) = ChainwebVersionName nid == _versionName v

-- | Check and assert that the number of signatures in a 'Command' is
-- at most 100.
--
assertSigSize :: [P.UserSig] -> Bool
assertSigSize sigs = length sigs <= defaultMaxCommandUserSigListSize

-- | Check and assert that the initial 'Gas' cost of a transaction
-- is less than the specified 'GasLimit'.
--
assertTxSize :: P.Gas -> P.GasLimit -> Bool
assertTxSize initialGas gasLimit = initialGas < fromIntegral gasLimit

-- | Check and assert that signers and user signatures are valid for a given
-- transaction hash.
--
assertValidateSigs :: [P.PPKScheme] -> IsWebAuthnPrefixLegal -> P.PactHash -> [P.Signer] -> [P.UserSig] -> Bool
assertValidateSigs validSchemes webAuthnPrefixLegal hsh signers sigs
    | length signers /= length sigs = False
    | otherwise = and $ zipWith verifyUserSig sigs signers
    where verifyUserSig sig signer =
            let
              sigScheme = fromMaybe P.ED25519 (P._siScheme signer)
              okScheme = sigScheme `elem` validSchemes
              okPrefix =
                webAuthnPrefixLegal == WebAuthnPrefixLegal ||
                not (P.webAuthnPrefix `T.isPrefixOf` P._siPubKey signer)
              okSignature = isRight $ P.verifyUserSig hsh sig signer
            in okScheme && okPrefix && okSignature

-- prop_tx_ttl_newBlock/validateBlock
--
-- Timing checks used to be based on the creation time of the validated
-- block. That changed on mainnet at block height 449940. Tx creation time
-- and TTL don't affect the tx outputs and pact state and can thus be
-- skipped when replaying old blocks.
--
assertTxTimeRelativeToParent
    :: ParentCreationTime
    -> P.Command (P.Payload P.PublicMeta c)
    -> Bool
assertTxTimeRelativeToParent (ParentCreationTime (BlockCreationTime txValidationTime)) tx =
    ttl > 0
    && txValidationTime >= timeFromSeconds 0
    && txOriginationTime >= 0
    && timeFromSeconds (txOriginationTime + ttl) > txValidationTime
    && P.TTLSeconds ttl <= defaultMaxTTL
  where
    P.TTLSeconds ttl = view cmdTimeToLive tx
    timeFromSeconds = Time . secondsToTimeSpan . Seconds . fromIntegral
    P.TxCreationTime txOriginationTime = view cmdCreationTime tx

-- | Check that the tx's creation time is not too far in the future relative
-- to the block creation time
assertTxNotInFuture
    :: ParentCreationTime
    -> P.Command (P.Payload P.PublicMeta c)
    -> Bool
assertTxNotInFuture (ParentCreationTime (BlockCreationTime txValidationTime)) tx =
    timeFromSeconds txOriginationTime <= lenientTxValidationTime
  where
    timeFromSeconds = Time . secondsToTimeSpan . Seconds . fromIntegral
    P.TxCreationTime txOriginationTime = view cmdCreationTime tx
    lenientTxValidationTime = add (scaleTimeSpan defaultLenientTimeSlop second) txValidationTime


-- | Assert that the command hash matches its payload and
-- its signatures are valid, without parsing the payload.
assertCommand :: P.Command PayloadWithText -> [P.PPKScheme] -> IsWebAuthnPrefixLegal -> Bool
assertCommand (P.Command pwt sigs hsh) ppkSchemePassList webAuthnPrefixLegal =
  isRight assertHash &&
  assertValidateSigs ppkSchemePassList webAuthnPrefixLegal hsh signers sigs
  where
    cmdBS = SBS.fromShort $ payloadBytes pwt
    signers = P._pSigners (payloadObj pwt)
    assertHash = P.verifyHash @'P.Blake2b_256 hsh cmdBS

-- -------------------------------------------------------------------- --
-- defaults

-- | The maximum admissible signature list size allowed for
-- Pact/Chainweb transactions
--
defaultMaxCommandUserSigListSize :: Int
defaultMaxCommandUserSigListSize = 100

-- | The maximum admissible number of decimal places allowed
-- by the coin contract.
--
defaultMaxCoinDecimalPlaces :: Word8
defaultMaxCoinDecimalPlaces = 12


-- | The maximum time-to-live (expressed in seconds)
--
-- This is probably going to be changed. Let us make it 2 days for now.
--
defaultMaxTTL :: P.TTLSeconds
defaultMaxTTL = P.TTLSeconds $ P.ParsedInteger $ 2 * 24 * 60 * 60

-- | Validation "slop" to allow for a more lenient creation time check after
-- @useLegacyCreationTimeForTxValidation@ is no longer true.
--
-- Without this, transactions showing up in the interim between
-- parent block issuance and new block creation can get rejected; the tradeoff reduces
-- the accuracy of the tx creation time vs "blockchain time", but is better than e.g.
-- incurring artificial latency to wait for a parent block that is acceptable for a tx.
-- 95 seconds represents the 99th percentile of block arrival times.
--
defaultLenientTimeSlop :: Seconds
defaultLenientTimeSlop = 95
