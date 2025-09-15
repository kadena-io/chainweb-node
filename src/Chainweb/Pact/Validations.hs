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
  assertPreflightMetadata
  -- * Validation checks
, assertChainId
, assertGasPrice
, assertNetworkId
, assertSigSize
, assertTxSize
, assertValidateSigs
, assertTxTimeRelativeToParent
, assertTxNotInFuture
, assertCommand
  -- * Defaults
, defaultMaxCommandUserSigListSize
, defaultMaxCoinDecimalPlaces
, defaultMaxTTLSeconds
, defaultLenientTimeSlop
) where

import Control.Lens

import Data.Decimal (decimalPlaces)
import Data.Maybe
import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Short as SBS
import Data.Word (Word8)

-- internal modules

import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.Pact.Types
import Chainweb.Parent
import Chainweb.Time (Seconds(..), Time(..), secondsToTimeSpan, scaleTimeSpan, second, add)
import Chainweb.Version

import qualified Pact.Core.Command.Types as Pact
import qualified Pact.Core.ChainData as Pact
import qualified Pact.Core.Gas.Types as Pact
import qualified Pact.Core.Hash as Pact
import qualified Chainweb.Pact.Transaction as Pact
import Chainweb.Utils (ebool_, int)
import Chainweb.Version.Guards (maxBlockGasLimit)
import Numeric.Natural


-- | Check whether a local Api request has valid metadata
--
assertPreflightMetadata
    :: HasVersion
    => ServiceEnv tbl
    -> Pact.Command (Pact.Payload Pact.PublicMeta c)
    -> BlockCtx
    -> Maybe LocalSignatureVerification
    -> Either (NonEmpty Text) ()
assertPreflightMetadata env cmd@(Pact.Command pay sigs hsh) blockCtx sigVerify = do
    let cid = view chainId env
    -- note that we use the maximum legal block gas limit here. if no miner is
    -- willing to accept this tx, it still may not make it on the chain.
    let mbgl = maxBlockGasLimit (_bctxCurrentBlockHeight blockCtx)

    let Pact.PublicMeta pcid _ gl gp _ _ = Pact._pMeta pay
        nid = Pact._pNetworkId pay
        signers = Pact._pSigners pay

    let errs = catMaybes
          [ eUnless "Chain id mismatch" $ assertChainId cid pcid
          -- TODO: use failing conversion
          , mbgl >>= \bgl ->
            eUnless "Transaction Gas limit exceeds block gas limit"
              $ assertBlockGasLimit (Pact.GasLimit $ Pact.Gas $ int @Natural @Pact.SatWord bgl) gl
          , eUnless "Gas price decimal precision too high" $ assertGasPrice gp
          , eUnless "Network id mismatch" $ assertNetworkId nid
          , eUnless "Signature list size too big" $ assertSigSize sigs
          , eUnless "Invalid transaction signatures" $ sigValidate signers
          , eUnless "Tx time outside of valid range" $ assertTxTimeRelativeToParent pct cmd
          ]

    case nonEmpty errs of
      Nothing -> Right ()
      Just vs -> Left vs
  where
    sigValidate signers
      | Just NoVerify <- sigVerify = True
      | otherwise = isRight $ assertValidateSigs hsh signers sigs

    pct = _bctxParentCreationTime blockCtx

    eUnless t assertion
      | assertion = Nothing
      | otherwise = Just t

-- | Check whether the chain id defined in the metadata of a Pact/Chainweb
-- command payload matches a given chain id.
--
-- The supplied chain id should be derived from the current
-- chainweb node structure
--
assertChainId :: ChainId -> Pact.ChainId -> Bool
assertChainId cid0 cid1 = chainIdToText cid0 == Pact._chainId cid1

-- | Check and assert that 'GasPrice' is rounded to at most 12 decimal
-- places.
--
assertGasPrice :: Pact.GasPrice -> Bool
assertGasPrice (Pact.GasPrice gp) = decimalPlaces gp <= defaultMaxCoinDecimalPlaces

-- | Check and assert that the 'GasLimit' of a transaction is less than or eqaul to
-- the block gas limit
--
assertBlockGasLimit :: Pact.GasLimit -> Pact.GasLimit -> Bool
assertBlockGasLimit bgl tgl = bgl >= tgl

-- | Check and assert that 'ChainwebVersion' is equal to some pact 'NetworkId'.
--
assertNetworkId :: HasVersion => Maybe Pact.NetworkId -> Bool
assertNetworkId Nothing = False
assertNetworkId (Just (Pact.NetworkId nid)) = ChainwebVersionName nid == _versionName implicitVersion

-- | Check and assert that the number of signatures in a 'Command' is
-- at most 100.
--
assertSigSize :: [Pact.UserSig] -> Bool
assertSigSize sigs = length sigs <= defaultMaxCommandUserSigListSize

-- | Check and assert that the initial 'Gas' cost of a transaction
-- is less than the specified 'GasLimit'.
--
assertTxSize :: Pact.Gas -> Pact.GasLimit -> Bool
assertTxSize initialGas gasLimit = Pact.GasLimit initialGas < gasLimit

-- | Check and assert that signers and user signatures are valid for a given
-- transaction hash.
--
assertValidateSigs :: ()
  => Pact.Hash
  -> [Pact.Signer]
  -> [Pact.UserSig]
  -> Either AssertValidateSigsError ()
assertValidateSigs hsh signers sigs = do
  let signersLength = length signers
  let sigsLength = length sigs
  ebool_
    SignersAndSignaturesLengthMismatch
        { _signersLength = signersLength
        , _signaturesLength = sigsLength
        }
    (signersLength == sigsLength)

  iforM_ (zip sigs signers) $ \pos (sig, signer) -> do
    case Pact.verifyUserSig hsh sig signer of
      Left errMsg -> Left (InvalidUserSig pos (Text.pack errMsg))
      Right () -> Right ()

-- prop_tx_ttl_newBlock/validateBlock
--
-- Timing checks used to be based on the creation time of the validated
-- block. That changed on mainnet at block height 449940. Tx creation time
-- and TTL don't affect the tx outputs and pact state and can thus be
-- skipped when replaying old blocks.
--
assertTxTimeRelativeToParent
    :: Parent BlockCreationTime
    -> Pact.Command (Pact.Payload Pact.PublicMeta c)
    -> Bool
assertTxTimeRelativeToParent (Parent (BlockCreationTime txValidationTime)) tx =
    ttl > 0
    && txValidationTime >= timeFromSeconds 0
    && txOriginationTime >= 0
    && timeFromSeconds (txOriginationTime + ttl) > txValidationTime
    && ttl <= defaultMaxTTLSeconds
  where
    Pact.TTLSeconds ttl = view (Pact.cmdPayload . Pact.pMeta . Pact.pmTTL) tx
    timeFromSeconds = Time . secondsToTimeSpan . Seconds . fromIntegral
    Pact.TxCreationTime txOriginationTime = view (Pact.cmdPayload . Pact.pMeta . Pact.pmCreationTime) tx

-- | Check that the tx's creation time is not too far in the future relative
-- to the block creation time
assertTxNotInFuture
    :: Parent BlockCreationTime
    -> Pact.Command (Pact.Payload Pact.PublicMeta c)
    -> Bool
assertTxNotInFuture (Parent (BlockCreationTime txValidationTime)) tx =
    timeFromSeconds txOriginationTime <= lenientTxValidationTime
  where
    timeFromSeconds = Time . secondsToTimeSpan . Seconds . fromIntegral
    Pact.TxCreationTime txOriginationTime = view (Pact.cmdPayload . Pact.pMeta . Pact.pmCreationTime) tx
    lenientTxValidationTime = add (scaleTimeSpan defaultLenientTimeSlop second) txValidationTime

-- | Assert that the command hash matches its payload and
-- its signatures are valid, without parsing the payload.
assertCommand :: Pact.Transaction -> Either AssertCommandError ()
assertCommand cmd = do
  _ <- assertHash & _Left .~ InvalidPayloadHash
  assertValidateSigs hsh signers (Pact._cmdSigs cmd) & _Left %~ AssertValidateSigsError
  where
    hsh = Pact._cmdHash cmd
    pwt = Pact._cmdPayload cmd
    cmdBS = SBS.fromShort $ pwt ^. Pact.payloadBytes
    signers = pwt ^. Pact.payloadObj . Pact.pSigners
    assertHash = Pact.verifyHash hsh cmdBS

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
defaultMaxTTLSeconds :: Integer
defaultMaxTTLSeconds = 2 * 24 * 60 * 60

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
