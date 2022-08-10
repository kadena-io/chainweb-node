{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Rosetta.Utils
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Linda Ortega <linda@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.Utils where

import Control.Monad (when)
import Control.Error.Util
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Foldable (foldl')
import Data.Decimal ( Decimal, DecimalRaw(Decimal) )
import Data.Hashable (Hashable(..))
import Data.List (sortOn, inits)
import Data.Word (Word64)
import Text.Read (readMaybe)
import Text.Printf ( printf )

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Memory.Endian as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.RPC as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Scheme as P
import qualified Pact.Parse as P
import qualified Pact.Types.Crypto as P
import qualified Data.Set as S
import Data.Maybe ( fromMaybe )
import qualified Pact.Types.RowData as P

import Numeric.Natural ( Natural )

import Pact.Types.Command
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.Exp (Literal(..))

import Rosetta

-- internal modules

import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.BlockHash ( blockHashToText )
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Pact.Utils
import Chainweb.Time
import Chainweb.Utils ( sshow, int, T2(..) )
import Chainweb.Version

---


--------------------------------------------------------------------------------
-- Rosetta Metadata Types --
--------------------------------------------------------------------------------

-- | Helper typeclass for transforming Rosetta metadata into a
--   JSON Object.
-- NOTE: Rosetta types expect metadata to be `Object`
class ToObject a where
  toPairs :: a -> [(T.Text, Value)]
  toObject :: a -> Object


data OperationMetaData = OperationMetaData
  { _operationMetaData_prevOwnership :: !Value
  , _operationMetaData_currOwnership :: !Value --TODO: hack for rotation bug
  } deriving Show
-- TODO: document
instance ToObject OperationMetaData where
  toPairs (OperationMetaData prevOwnership currOwnership) =
    [ "prev-ownership" .= prevOwnership
    , "curr-ownership" .= currOwnership ]
  toObject opMeta = HM.fromList (toPairs opMeta)
instance FromJSON OperationMetaData where
  parseJSON = withObject "OperationMetaData" $ \o -> do
    prevOwnership <- o .: "prev-ownership"
    currOwnership <- o .: "curr-ownership"
    pure OperationMetaData
      { _operationMetaData_prevOwnership = prevOwnership
      , _operationMetaData_currOwnership = currOwnership
      }

-- TODO: Not currently used because of ownership rotation bug.
newtype AccountIdMetaData = AccountIdMetaData
  { _accountIdMetaData_currOwnership :: Value }
  deriving Show
instance ToObject AccountIdMetaData where
  toPairs (AccountIdMetaData currOwnership) =
    [ "current-ownership" .= currOwnership ]
  toObject acctMeta = HM.fromList (toPairs acctMeta)
instance FromJSON AccountIdMetaData where
  parseJSON = withObject "AccountIdMetaData" $ \o -> do
    currOwnership <- o .: "current-ownership"
    pure AccountIdMetaData {
      _accountIdMetaData_currOwnership = currOwnership
    }

newtype TransactionMetaData = TransactionMetaData
  { _transactionMetaData_multiStepTx :: Maybe ContinuationMetaData
  }
instance ToObject TransactionMetaData where
  toPairs (TransactionMetaData Nothing) = []
  toPairs (TransactionMetaData (Just multi)) =
    [ "multi-step-transaction" .= toObject multi ]
  toObject txMeta = HM.fromList (toPairs txMeta)

transactionMetaData :: ChainId -> CommandResult a -> TransactionMetaData
transactionMetaData cid cr = case _crContinuation cr of
  Nothing -> TransactionMetaData Nothing
  Just pe -> TransactionMetaData $ Just (toContMeta cid pe)


-- | Adds more transparency into a continuation transaction
-- that was just executed.
data ContinuationMetaData = ContinuationMetaData
  { _continuationMetaData_currStep :: !ContinuationCurrStep
  -- ^ Information on the current step in the continuation.
  , _continuationStep_nextStep :: !(Maybe ContinuationNextStep)
  -- ^ Information on the next step in the continuation (if there is one).
  , _continuationMetaData_pactIdReqKey :: !P.PactId
  -- ^ The request key of the transaction that initiated this continuation.
  -- TODO: Further work needs to be done to know WHICH chain this
  --       initial transaction occurred in.
  , _continuationMetaData_totalSteps :: !Int
  -- ^ Total number of steps in the entire continuation.
  } deriving Show
-- TODO: document
instance ToObject ContinuationMetaData where
  toPairs (ContinuationMetaData curr next rk total) =
    [ "current-step" .= toObject curr
    , "first-step-request-key" .= rk
    , "total-steps" .= total ]
    <> omitNextIfMissing
    where
      omitNextIfMissing = case next of
        Nothing -> []
        Just ns -> [ "next-step" .= toObject ns ]
  toObject contMeta = HM.fromList (toPairs contMeta)

toContMeta :: ChainId -> P.PactExec -> ContinuationMetaData
toContMeta cid pe = ContinuationMetaData
  { _continuationMetaData_currStep = toContStep cid pe
  , _continuationStep_nextStep = toContNextStep cid pe
  , _continuationMetaData_pactIdReqKey = P._pePactId pe
  , _continuationMetaData_totalSteps = P._peStepCount pe
  }


-- | Provides information on the continuation step that was just executed.
data ContinuationCurrStep = ContinuationCurrStep
  { _continuationCurrStep_chainId :: !T.Text
  -- ^ Chain id where step was executed
  , _continuationCurrStep_stepId :: !Int
  -- ^ Step that was executed or skipped
  , _continuationCurrStep_rollbackAvailable :: Bool
  -- ^ Track whether a current step allows for rollbacks
  } deriving Show
-- TODO: Add ability to detect if step was rolled back.
-- TODO: document
instance ToObject ContinuationCurrStep where
  toPairs (ContinuationCurrStep cid step rollback) =
    [ "chain-id" .= cid
    , "step-id" .= step
    , "rollback-available" .= rollback ]
  toObject contCurrStep = HM.fromList (toPairs contCurrStep)

toContStep :: ChainId -> P.PactExec -> ContinuationCurrStep
toContStep cid pe = ContinuationCurrStep
  { _continuationCurrStep_chainId = chainIdToText cid
  , _continuationCurrStep_stepId = P._peStep pe
  , _continuationCurrStep_rollbackAvailable = P._peStepHasRollback pe
  }


-- | Indicates if the next step of a continuation occurs in a
--   different chain or in the same chain.
newtype ContinuationNextStep = ContinuationNextStep
  { _continuationNextStep_chainId :: T.Text
  } deriving Show
-- TODO: document
instance ToObject ContinuationNextStep where
  toPairs (ContinuationNextStep cid) = [ "target-chain-id" .= cid ]
  toObject contNextStep = HM.fromList (toPairs contNextStep)

-- | Determines if the continuation has a next step and, if so, provides
--   the chain id of where this next step will need to occur.
toContNextStep
    :: ChainId
    -> P.PactExec
    -> Maybe ContinuationNextStep
toContNextStep currChainId pe
  | isLastStep = Nothing
  -- TODO: Add check to see if curr step was rolled back.
  --       This would also mean a next step is not occuring.
  | otherwise = case P._peYield pe >>= P._yProvenance of
      Nothing -> Just $ ContinuationNextStep $ chainIdToText currChainId
      -- next step occurs in the same chain
      Just (P.Provenance nextChainId _) ->
      -- next step is a cross-chain step
        Just $ ContinuationNextStep (P._chainId nextChainId)
  where
    isLastStep = succ $ P._peStep pe == P._peStepCount pe

--------------------------------------------------------------------------------
-- Rosetta ConstructionAPI Types and Helper Functions --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- /preprocess
data PreprocessReqMetaData = PreprocessReqMetaData
  { _preprocessReqMetaData_gasPayer :: !AccountId
  , _preprocessReqMetaData_nonce :: !(Maybe T.Text)
  } deriving (Show, Eq)
instance ToObject PreprocessReqMetaData where
  toPairs (PreprocessReqMetaData payer someNonce) =
    toPairOmitMaybe
    [ "gas_payer" .= payer ]
    [ maybePair "nonce" someNonce ]
  toObject meta = HM.fromList (toPairs meta)
instance ToJSON PreprocessReqMetaData where
  toJSON = object . toPairs
instance FromJSON PreprocessReqMetaData where
  parseJSON = withObject "PreprocessReqMetaData" $ \o -> do
    payer <- o .: "gas_payer"
    nonce <- o .:? "nonce"
    _ <- case rosettaAccountIdtoKAccount payer of
      Left errMsg -> error $ show errMsg
      Right _ -> pure ()
    return $ PreprocessReqMetaData
      { _preprocessReqMetaData_gasPayer = payer
      , _preprocessReqMetaData_nonce = nonce
      }


-- | The different types of pact coin-contract transactions allowed in
-- Construction API. Used in the response of both /preprocess and /metadata endpoints.
-- NOTE: Only KeySet guards are considered for simplicity.
data ConstructionTx =
    ConstructTransfer
      { _constructTransfer_from :: !AccountId
      , _constructTransfer_fromGuard :: !P.KeySet
      , _constructTransfer_to :: !AccountId
      , _constructTransfer_toGuard :: !P.KeySet
      , _constructTransfer_amount :: !P.ParsedDecimal
      }
  deriving (Show, Eq)
instance ToJSON ConstructionTx where
  toJSON (ConstructTransfer from fromGuard to toGuard amt) =
    object [ "tx_type" .= ("transfer" :: T.Text)
           , "sender_account" .= from
           , "sender_ownership" .= fromGuard
           , "receiver_account" .= to
           , "receiver_ownership" .= toGuard
           , "transfer_amount" .= amt ]
instance FromJSON ConstructionTx where
  parseJSON = withObject "ConstructionTx" $ \o -> do
    typ :: T.Text <- o .: "tx_type"
    case typ of
      "transfer" -> parseTransfer o
      _ -> error $ "Invalid ConstructionTx 'tx_type' value: " ++ show typ
    where
      parseTransfer o = do
        from <- o .: "sender_account"
        fromGuard <- o .: "sender_ownership"
        to <- o .: "receiver_account"
        toGuard <- o .: "receiver_ownership"
        amt <- o .: "transfer_amount"
        let actualTx = ConstructTransfer
              { _constructTransfer_from = from
              , _constructTransfer_fromGuard = fromGuard
              , _constructTransfer_to = to
              , _constructTransfer_toGuard = toGuard
              , _constructTransfer_amount = amt
              }
            from' = (from, negate amt, fromGuard)
            to' = (to, amt, toGuard)
        case transferTx from' to' of
          Left errMsg -> error $ show errMsg
          Right expectedTx
            | expectedTx == actualTx -> pure actualTx
            | otherwise -> error $
              "Expected ConstructionTx: " <> show expectedTx <>
              "/n but received: " <> show actualTx

-- Constructs a Transfer ConstructionTx and
-- performs balance and k:account checks.
transferTx
    :: (AccountId, P.ParsedDecimal, P.KeySet)
    -> (AccountId, P.ParsedDecimal, P.KeySet)
    -> Either RosettaError ConstructionTx
transferTx (acct1, bal1, ks1) (acct2, bal2, ks2)
  | acct1 == acct2 =
    rerr RosettaInvalidOperations
         "Cannot transfer to the same account name"
  -- Enforce accounts are valid k accounts
  | not (validateKAccount $ _accountId_address acct1) =
    rerr RosettaInvalidKAccount
          (show acct1)
  | not (validateKAccount $ _accountId_address acct2) =
    rerr RosettaInvalidKAccount
          (show acct2)
  | not (validateKAccountKeySet (_accountId_address acct1) ks1) =
    rerr RosettaInvalidKAccount $
        "Invalid KeySet: " <> show ks1
  | not (validateKAccountKeySet (_accountId_address acct2) ks2) =
    rerr RosettaInvalidKAccount $
        "Invalid KeySet: " <> show ks2
  -- Perform balance checks
  | bal1 + bal2 /= 0.0 =
    rerr RosettaInvalidOperations
        "transfer amounts: Mass conversation not preserved"
  | bal1 == 0 || bal2 == 0 =
    rerr RosettaInvalidOperations
        "transfer amounts: Cannot transfer zero amounts"
  | bal1 < 0.0 = pure $ ConstructTransfer
    { _constructTransfer_from = acct1    -- bal1 is negative, so acct1 is debitor (from)
    , _constructTransfer_fromGuard = ks1
    , _constructTransfer_to = acct2      -- bal2 is positive, so acct2 is creditor (to)
    , _constructTransfer_toGuard = ks2
    , _constructTransfer_amount = abs bal1
    }
  | otherwise = pure $ ConstructTransfer
    { _constructTransfer_from = acct2    -- bal2 is negative, so acct2 is debitor (from)
    , _constructTransfer_fromGuard = ks2
    , _constructTransfer_to = acct1      -- bal1 is positive, so acct1 is creditor (to)
    , _constructTransfer_toGuard = ks1
    , _constructTransfer_amount = abs bal1
    }
  where
    rerr f msg = Left $ stringRosettaError f msg

newtype DeriveRespMetaData = DeriveRespMetaData
  { _deriveRespMetaData_ownership :: P.KeySet }
instance ToObject DeriveRespMetaData where
  toPairs (DeriveRespMetaData ownership) =
    [ "ownership" .= ownership ]
  toObject m = HM.fromList (toPairs m)
instance FromJSON DeriveRespMetaData where
  parseJSON = withObject "DeriveRespMetaData" $ \o -> do
    ownership <- o .: "ownership"
    return DeriveRespMetaData
      { _deriveRespMetaData_ownership = ownership }

data PreprocessRespMetaData = PreprocessRespMetaData
  { _preprocessRespMetaData_reqMetaData :: PreprocessReqMetaData
  , _preprocessRespMetaData_tx :: ConstructionTx
  , _preprocessRespMetaData_suggestedFee :: Amount
  , _preprocessRespMetaData_gasLimit :: P.GasLimit
  , _preprocessRespMetaData_gasPrice :: P.GasPrice
  } deriving (Show, Eq)
instance ToObject PreprocessRespMetaData where
  toPairs (PreprocessRespMetaData reqMeta txInfo fee gasLimit gasPrice) =
    [ "preprocess_request_metadata" .= reqMeta
    , "tx_info" .= txInfo
    , "suggested_fee" .= fee
    , "gas_limit" .= gasLimit
    , "gas_price" .= gasPrice ]
  toObject m = HM.fromList (toPairs m)
instance FromJSON PreprocessRespMetaData where
  parseJSON = withObject "PreprocessRespMetaData" $ \o -> do
    reqMeta <- o .: "preprocess_request_metadata"
    txInfo <- o .: "tx_info"
    fee <- o .: "suggested_fee"
    gasLimit <- o .: "gas_limit"
    gasPrice <- o .: "gas_price"
    return PreprocessRespMetaData
      { _preprocessRespMetaData_reqMetaData = reqMeta
      , _preprocessRespMetaData_tx = txInfo
      , _preprocessRespMetaData_suggestedFee = fee
      , _preprocessRespMetaData_gasLimit = gasLimit
      , _preprocessRespMetaData_gasPrice = gasPrice
      }


-- | Parse list of Operations into feasible Pact transactions.
-- NOTE: Expects that user-provided values are valid (i.e. AccountIds).
opsToConstructionTx
    :: [Operation]
    -> Either RosettaError ConstructionTx
opsToConstructionTx ops = do
  ops' <- mapM parseOp ops
  case ops' of
    [] -> rerr RosettaInvalidOperations
            "Found empty list of Operations"
    [op1, op2] -> transferTx op1 op2
    _ -> rerr RosettaInvalidOperations
           "Expected at MOST two operations"
  where
    rerr f msg = Left $ stringRosettaError f msg

-- | Calculate the suggested fee in KDA for the transaction to be performed.
-- Some optional parameters might be specified, i.e. a max KDA fee and a fee multiplier.
-- FORMULA: fee = gasLimit * (gasPrice * multiplier)
-- NOTE: The multiplier will be absorbed into the gasPrice since assuming that
--       the higher the gasPrice the more likely the transaction will be added to a block.
-- Specifications: https://www.rosetta-api.org/docs/1.4.4/models/ConstructionPreprocessRequest.html
getSuggestedFee
    :: ConstructionTx
    -> Maybe [Amount]
    -> Maybe Double
    -> Either RosettaError (P.GasLimit, P.GasPrice, Amount)
getSuggestedFee tx someMaxFees someMult = do
  someMaxFee <- parseMaxFees someMaxFees
  mapM_ checkMaxFeeSufficient someMaxFee
  let gasLimit = estimatedGasLimit
      someMaxPrice = fmap (calcMaxGasPrice gasLimit) someMaxFee
      gasPrice = calcGasPrice someMaxPrice
      fee = kdaToRosettaAmount $! calcKDAFee gasLimit gasPrice

  pure (gasLimit, gasPrice, fee)

  where
    ------------
    -- Defaults
    ------------
    -- NOTE: GasLimit should never be greater than default block gas limit.

    -- Derived from a couple of gas unit cost of the following transfer transactions + some buffer:
    -- - https://explorer.chainweb.com/testnet/tx/g8dxg1CAM3eZ5S-rk51N27N8-nKEW3Wg_cyk5moqmBg
    -- - https://explorer.chainweb.com/testnet/tx/IGVzaRkTHOSMIiHM7q8bPxrATW5b5SEhoCqE6tPkVFA
    -- - https://explorer.chainweb.com/testnet/tx/cK0B0XOkOlMDR32GloR0GQvjAWAJ9mvNPZwQDalPr6c
    defGasUnitsTransferCreate = 2500

    -- See Chainweb.Chainweb.Configuration for latest min gas
    minGasPrice = Decimal 8 1

    -------------------
    -- Helper Functions
    -------------------
    -- Assumption: Currency is in KDA
    parseMaxFees :: Maybe [Amount] -> Either RosettaError (Maybe Decimal)
    parseMaxFees Nothing = pure Nothing
    parseMaxFees (Just []) = pure Nothing
    parseMaxFees (Just [a]) = do
      dec <- parseAmount a
      checkIfPositive dec
      pure $ Just dec
      where
        checkIfPositive d
          | d >= 0 = pure ()
          | otherwise =
            Left $ stringRosettaError RosettaInvalidAmount
              "max_fee: Expected positive Amount"
    parseMaxFees _ =
      Left $ stringRosettaError RosettaInvalidAmount
        "max_fee: Expected single Amount, but found multiple Amounts."

    -- Make sure that the max fee is sufficent to cover the cost of
    -- the specified transaction at the minimum gas price.
    checkMaxFeeSufficient :: Decimal -> Either RosettaError ()
    checkMaxFeeSufficient maxFee
      | maxFee >= minFeeNeeded = pure ()
      | otherwise =
        Left $ stringRosettaError RosettaInvalidAmount $
          "max_fee: Expected a minimum fee of " ++ show minFeeNeeded ++
          "KDA for specified operations, but received max_fee=" ++ show maxFee ++ "KDA"
      where
        minFeeNeeded =
          calcKDAFee estimatedGasLimit
            (P.GasPrice $ P.ParsedDecimal minGasPrice)

    estimatedGasLimit = P.GasLimit $ P.ParsedInteger $! case tx of
      ConstructTransfer {} -> defGasUnitsTransferCreate

    -- Calculate the maximum gas price possible give the max fee provided and the
    -- needed gas units for the specified transaction.
    -- NOTE: The max fee acts as upper bound on the suggested fee
    --       (regardless of the multiplier provided).
    calcMaxGasPrice :: P.GasLimit -> Decimal -> Decimal
    calcMaxGasPrice gasLimit maxFee = fromIntegral maxGasPrice
      where
        P.GasLimit (P.ParsedInteger units) = gasLimit
        maxGasPrice :: Integer = floor (maxFee / fromInteger units)

    -- Sanitize the fee multiplier provided by user.
    -- Since the multiplier will multiplied into the min gas price,
    -- (1) Makes sure that it's not zero.
    -- (2) Makes sure that it's above 1.0, otherwise the final gas price
    --     will be lower than the min precision allowed in Pact.
    mult :: Decimal
    mult = realToFrac $
      case someMult of
        Nothing -> 1.0
        Just m
          | m <= 1.0 -> 1.0
          | otherwise -> m

    calcGasPrice :: Maybe Decimal -> P.GasPrice
    calcGasPrice someMaxGasPrice = P.GasPrice $ P.ParsedDecimal $!
      case someMaxGasPrice of
        Nothing -> minGasPrice * mult  -- no max fee provided
        Just maxGasPrice
          | (minGasPrice * mult) > maxGasPrice -> maxGasPrice
          | otherwise -> minGasPrice * mult

    calcKDAFee :: P.GasLimit -> P.GasPrice -> Decimal
    calcKDAFee gasLimit gasPrice = fee
      where
        P.GasLimit (P.ParsedInteger units) = gasLimit
        P.GasPrice (P.ParsedDecimal price) = gasPrice
        fee = fromIntegral units * price


--------------------------------------------------------------------------------
-- /metadata

toPublicMeta
    :: ChainId
    -> AccountId
    -> P.GasLimit
    -> P.GasPrice
    -> IO P.PublicMeta
toPublicMeta cid acct gasLimit gasPrice = do
  creationTime <- toTxCreationTime <$> getCurrentTimeIntegral

  pure $ P.PublicMeta
    { P._pmChainId = P.ChainId $ chainIdToText cid
    , P._pmSender = _accountId_address acct
    , P._pmGasLimit = gasLimit
    , P._pmGasPrice = gasPrice
    , P._pmTTL = defaultTransactionTTL
    , P._pmCreationTime = creationTime
    }
  where
    defaultTransactionTTL = P.TTLSeconds (8 * 60 * 60) -- 8 hours


toNonce :: Maybe T.Text -> P.PublicMeta -> T.Text
toNonce (Just nonce) _ = nonce
toNonce Nothing pm = sshow $! P._pmCreationTime pm

rosettaAccountIdtoKAccount :: AccountId -> Either RosettaError (T2 T.Text P.KeySet)
rosettaAccountIdtoKAccount acct = do
  let kAccount = _accountId_address acct
  ownership <- toRosettaError RosettaInvalidKAccount $
              note (show acct) $
              generateKeySetFromKAccount kAccount
  pure $! T2 kAccount ownership

rosettaPubKeyTokAccount :: RosettaPublicKey -> Either RosettaError (T2 T.Text P.KeySet)
rosettaPubKeyTokAccount (RosettaPublicKey pubKey curve) = do
  _ <- getScheme curve -- enforce only valid schemes
  let pubKeyPact = P.PublicKey $ T.encodeUtf8 pubKey
  kAccount <- toRosettaError RosettaInvalidPublicKey $
              note (show pubKey) $
              generateKAccountFromPubKey pubKeyPact
  let ownership = pubKeyToKAccountKeySet pubKeyPact
  pure $! T2 kAccount ownership

toPactPubKeyAddr
    :: T.Text
    -> P.PPKScheme
    -> Either RosettaError T.Text
toPactPubKeyAddr pk sk = do
  let scheme = P.toScheme sk
  bs <- toRosettaError RosettaInvalidPublicKey $! P.parseB16TextOnly pk
  addrBS <- toRosettaError RosettaInvalidPublicKey $!
            P.formatPublicKeyBS scheme (P.PubBS bs)
  pure $! P.toB16Text addrBS


signerToAddr :: Signer -> Either RosettaError T.Text
signerToAddr (Signer someScheme pk someAddr _) = do
  let sk = fromMaybe P.ED25519 someScheme
      addr = fromMaybe pk someAddr
  toPactPubKeyAddr addr sk


getScheme :: CurveType -> Either RosettaError P.PPKScheme
getScheme CurveEdwards25519 = pure P.ED25519
getScheme ct = Left $ stringRosettaError RosettaInvalidPublicKey $
               "Found unsupported CurveType: " ++ show ct

sigToScheme :: RosettaSignatureType -> Either RosettaError P.PPKScheme
sigToScheme RosettaEd25519 = pure P.ED25519
sigToScheme st = Left $ stringRosettaError RosettaInvalidSignature $
                 "Found unsupported SignatureType: " ++ show st


newtype AccountName = AccountName { _accountName :: T.Text }
  deriving (Show, Eq, Ord)
instance Hashable AccountName where
  hash (AccountName n) = hash n
  hashWithSalt i (AccountName n) = hashWithSalt i n

-- TODO: If AccountId metadata changes to include the account guard,
-- will need to ask for keyset here.
acctNameToAcctId :: AccountName -> AccountId
acctNameToAcctId (AccountName name) = accountId name


--------------------------------------------------------------------------------
-- /payloads

data PayloadsMetaData = PayloadsMetaData
  { _payloadsMetaData_signers :: ![(Signer, AccountId)]
  , _payloadsMetaData_nonce :: !T.Text
  , _payloadsMetaData_publicMeta :: !P.PublicMeta
  , _payloadsMetaData_tx :: !ConstructionTx
  -- ^ Needed to construct gas payer AccountId
  } deriving (Show)
instance ToObject PayloadsMetaData where
  toPairs (PayloadsMetaData signers nonce pm tx) =
    [ "signers" .= signers
    , "nonce" .= nonce
    , "public_meta" .= pm
    , "tx" .= tx
    ]
  toObject m = HM.fromList (toPairs m)
instance FromJSON PayloadsMetaData where
  parseJSON = withObject "PayloadsMetaData" $ \o -> do
    signers <- o .: "signers"
    nonce <- o .: "nonce"
    publicMeta <- o .: "public_meta"
    tx <- o .: "tx"
    pure PayloadsMetaData
      {  _payloadsMetaData_signers = signers
      , _payloadsMetaData_nonce = nonce
      , _payloadsMetaData_publicMeta = publicMeta
      , _payloadsMetaData_tx = tx
      }


data EnrichedCommand = EnrichedCommand
  { _enrichedCommand_cmd :: !(Command T.Text)
  , _enrichedCommand_txInfo :: !ConstructionTx
  , _enrichedCommand_signerAccounts :: ![AccountId]
  } deriving (Show)
instance ToJSON EnrichedCommand where
  toJSON (EnrichedCommand cmd tx accts) = object
    [ "cmd" .= cmd
    , "tx_info" .= tx
    , "signer_accounts" .= accts ]
instance FromJSON EnrichedCommand where
  parseJSON = withObject "EnrichedCommand" $ \o -> do
    cmd <- o .: "cmd"
    txInfo <- o .: "tx_info"
    accts <- o .: "signer_accounts"
    pure EnrichedCommand
      { _enrichedCommand_cmd = cmd
      , _enrichedCommand_txInfo = txInfo
      , _enrichedCommand_signerAccounts = accts
      }


enrichedCommandToText :: EnrichedCommand -> T.Text
enrichedCommandToText = T.decodeUtf8 . BSL.toStrict . encode

textToEnrichedCommand :: T.Text -> Maybe EnrichedCommand
textToEnrichedCommand = decodeStrict' . T.encodeUtf8

transferCreateCode :: AccountId -> (AccountId, P.KeySet) -> P.ParsedDecimal -> (T.Text, Value)
transferCreateCode from (to, toGuard) amt =
  let code = T.pack $! printf
            "(coin.transfer-create %s %s (read-keyset %s) (read-decimal %s))"
            (acctTostr from) (acctTostr to) (show guardName) (show amountName)
      rdata = object
            [ guardName .= toGuard
            , amountName .= amt ]
  in (code, rdata)
  where
    acctTostr = show . T.unpack . _accountId_address
    amountName :: T.Text = "amount"
    guardName :: T.Text = "ks"

constructionTxToPactRPC
    :: ConstructionTx
    -> P.PactRPC T.Text
constructionTxToPactRPC txInfo =
  case txInfo of
    ConstructTransfer from _ to toGuard amt ->
      let (code, rdata) = transferCreateCode from (to, toGuard) amt
      in P.Exec $ P.ExecMsg code rdata


-- | Creates an enriched Command that consists of an
--   unsigned Command object, as well as any extra information lost
--   when constructing the command but needed in the /parse
--   endpoint.
createUnsignedCmd :: ChainwebVersion -> PayloadsMetaData -> IO EnrichedCommand
createUnsignedCmd v meta = do
  cmd <- mkUnsignedCommand pactSigners pubMeta nonce networkId pactRPC
  let cmdText = T.decodeUtf8 <$> cmd
  pure $ EnrichedCommand cmdText txInfo signerAccts
  where
    PayloadsMetaData signers nonce pubMeta txInfo = meta
    signerAccts = map snd signers
    pactSigners = map fst signers
    networkId = Just $ P.NetworkId $! chainwebVersionToText v
    pactRPC = constructionTxToPactRPC txInfo


createSigningPayloads
    :: EnrichedCommand
    -> [(Signer, AccountId)]
    -> [RosettaSigningPayload]
createSigningPayloads (EnrichedCommand cmd _ _) = map f
  where
    hashBase16 = P.toB16Text $! P.unHash $!
                 P.toUntypedHash $! _cmdHash cmd

    f (signer, acct) = RosettaSigningPayload
      { _rosettaSigningPayload_address = Nothing
      , _rosettaSigningPayload_accountIdentifier = Just acct
      , _rosettaSigningPayload_hexBytes = hashBase16
      , _rosettaSigningPayload_signatureType = toRosettaSigType $ _siScheme signer
      }

    toRosettaSigType Nothing = Just RosettaEd25519
    toRosettaSigType (Just P.ED25519) = Just RosettaEd25519
    toRosettaSigType (Just P.ETH) = Just RosettaEcdsa -- TODO: unsupport this

--------------------------------------------------------------------------------
-- /parse

txToOps :: ConstructionTx -> [Operation]
txToOps txInfo = case txInfo of
  ConstructTransfer from fromGuard to toGuard (P.ParsedDecimal amt) ->
    [ op (_accountId_address from) (negate amt) fromGuard 0
    , op (_accountId_address to) amt toGuard 1
    ]

  where
    op name delta guard idx =
      o { _operation_status = "" }
      -- validator expects empty op status
      where o = operation
                Successful
                TransferOrCreateAcct
                (toAcctLog name delta guard)
                idx
                []

    toAcctLog name delta guard = AccountLog
      { _accountLogKey = name
      , _accountLogBalanceDelta = BalanceDelta delta
      , _accountLogCurrGuard = toJSON guard
      , _accountLogPrevGuard = toJSON guard
      }


--------------------------------------------------------------------------------
-- /combine

getCmdPayload
    :: Command T.Text
    -> Either RosettaError (Payload P.PublicMeta T.Text)
getCmdPayload (Command p _ _) =
  note (rosettaError' RosettaUnparsableTx)
    (decodeStrict' $! T.encodeUtf8 p)


matchSigs
    :: [RosettaSignature]
    -> [Signer]
    -> Either RosettaError [UserSig]
matchSigs sigs signers = do
  sigMap <- HM.fromList <$> mapM sigAndAddr sigs
  mapM (match sigMap) signers

  where
    match
        :: HM.HashMap T.Text UserSig
        -> Signer
        -> Either RosettaError UserSig
    match m signer = do
      addr <- signerToAddr signer
      note (stringRosettaError RosettaInvalidSignature
            $ "Missing signature for public key=" ++ show (_siPubKey signer))
            $ HM.lookup addr m

    sigAndAddr (RosettaSignature _ (RosettaPublicKey pk ct) sigTyp sig) = do
      _ <- toRosettaError RosettaInvalidSignature $! P.parseB16TextOnly sig
      sigScheme <- sigToScheme sigTyp
      pkScheme <- getScheme ct
      when (sigScheme /= pkScheme)
        (Left $ stringRosettaError RosettaInvalidSignature $
         "Expected the same Signature and PublicKey type for Signature=" ++ show sig)

      let userSig = P.UserSig sig
      addr <- toPactPubKeyAddr pk pkScheme
      pure (addr, userSig)

--------------------------------------------------------------------------------
-- Rosetta Helper Types --
--------------------------------------------------------------------------------

type CoinbaseTx chainwebTx = chainwebTx
newtype BalanceDelta = BalanceDelta { _balanceDelta :: Decimal }
  deriving (Show, Eq)
data AccountLog = AccountLog
  { _accountLogKey :: !T.Text
  , _accountLogBalanceDelta :: !BalanceDelta
  , _accountLogCurrGuard :: !Value
  , _accountLogPrevGuard :: !Value
  }
  deriving (Show, Eq)
type AccountRow = (T.Text, Decimal, Value)

-- | An operation index and related operations can only be
--   determined once all operations in a transaction are known.
type UnindexedOperation =
     Word64
  -- ^ Operation index
  -> [OperationId]
  -- ^ Id of Related Operations
  -> Operation

data UnindexedOperations = UnindexedOperations
  { _unindexedOperation_fundOps :: [UnindexedOperation]
  , _unindexedOperation_transferOps :: [UnindexedOperation]
  , _unindexedOperation_gasOps :: [UnindexedOperation]
  }

data ChainwebOperationStatus = Successful | Remediation
  deriving (Enum, Bounded, Show)

data OperationType =
    CoinbaseReward
  | FundTx
  | GasPayment
  | TransferOrCreateAcct
  deriving (Enum, Bounded, Show)


--------------------------------------------------------------------------------
-- Functions to create Rosetta types --
--------------------------------------------------------------------------------

-- | If its the genesis block, Rosetta wants the parent block to be itself.
--   Otherwise, fetch the parent header from the block.
parentBlockId :: BlockHeader -> BlockId
parentBlockId bh
  | bHeight == genesisHeight v cid = blockId bh  -- genesis
  | otherwise = parent
  where
    bHeight = _blockHeight bh
    cid = _blockChainId bh
    v = _blockChainwebVersion bh
    parent = BlockId
      { _blockId_index = _height (pred $ _blockHeight bh)
      , _blockId_hash = blockHashToText (_blockParent bh)
      }

blockId :: BlockHeader -> BlockId
blockId bh = BlockId
  { _blockId_index = _height (_blockHeight bh)
  , _blockId_hash = blockHashToText (_blockHash bh)
  }

cmdToTransactionId :: Command T.Text -> TransactionId
cmdToTransactionId = TransactionId . requestKeyToB16Text . cmdToRequestKey

rosettaTransactionFromCmd :: Command a -> [Operation] -> Transaction
rosettaTransactionFromCmd cmd ops =
  Transaction
    { _transaction_transactionId = pactHashToTransactionId (_cmdHash cmd)
    , _transaction_operations = ops
    , _transaction_metadata = Nothing
    }

rosettaTransaction :: CommandResult a -> ChainId -> [Operation] -> Transaction
rosettaTransaction cr cid ops =
  Transaction
    { _transaction_transactionId = rkToTransactionId (_crReqKey cr)
    , _transaction_operations = ops
    , _transaction_metadata = Just $ toObject (transactionMetaData cid cr)
    }

pactHashToTransactionId :: P.PactHash -> TransactionId
pactHashToTransactionId hsh = TransactionId $ P.hashToText $ P.toUntypedHash hsh

rkToTransactionId :: RequestKey -> TransactionId
rkToTransactionId rk = TransactionId $ requestKeyToB16Text rk

accountId :: T.Text -> AccountId
accountId acctName = AccountId
  { _accountId_address = acctName
  , _accountId_subAccount = Nothing  -- assumes coin acct contract only
  , _accountId_metadata = Nothing -- disabled due to ownership rotation bug
  }
  where
    _accountIdMeta = Nothing

operationStatus :: ChainwebOperationStatus -> OperationStatus
operationStatus s@Successful =
  OperationStatus
    { _operationStatus_status = sshow s
    , _operationStatus_successful = True
    }
operationStatus s@Remediation =
  OperationStatus
    { _operationStatus_status = sshow s
    , _operationStatus_successful = True
    }

-- | Flatten operations grouped by TxId into a single list of operations;
--   give each operation a unique, numerical operation id based on its position
--   in this new flattened list; and create DAG of related operations.
indexedOperations :: UnindexedOperations -> [Operation]
indexedOperations unIdxOps = fundOps <> transferOps <> gasOps
  where
    opIds = map _operation_operationId

    createOps opsF begIdx defRelatedOpIds =
      let ops = zipWith (\f i -> f i defRelatedOpIds) opsF [begIdx..]
      in weaveRelatedOperations $! ops
      -- connect operations to each other

    fundUnIdxOps = _unindexedOperation_fundOps $! unIdxOps
    fundOps = createOps fundUnIdxOps 0 []

    transferIdx = fromIntegral $! length fundOps
    transferUnIdxOps = _unindexedOperation_transferOps $! unIdxOps
    transferOps = createOps transferUnIdxOps transferIdx []

    gasIdx = transferIdx + (fromIntegral $! length transferOps)
    gasUnIdxOps = _unindexedOperation_gasOps $! unIdxOps
    gasOps = createOps gasUnIdxOps gasIdx (opIds $! fundOps)
    -- connect gas operations to fund operations

-- | Create a DAG of related operations.
-- Algorithm:
--   Given a list of operations that are related:
--     For operation x at position i,
--       Overwrite or append all operations ids at
--         position 0th to ith (not inclusive) to operation x's
--         related operations list.
-- Example: list of operations to weave: [ 4: [], 5: [1], 6: [] ]
--          weaved operations: [ 4: [], 5: [1, 4], 6: [4, 5] ]
weaveRelatedOperations :: [Operation] -> [Operation]
weaveRelatedOperations relatedOps = map weave opsWithRelatedOpIds
  where
    -- example: [1, 2, 3] -> [[], [1], [1,2], [1,2,3]]
    opIdsDAG = inits $! map _operation_operationId relatedOps
    -- example: [(op 1, []), (op 2, [1]), (op 3, [1,2])]
    opsWithRelatedOpIds = zip relatedOps opIdsDAG

    -- related operation ids must be in descending order.
    justSortRelated r = Just $! sortOn _operationId_index r

    weave (op, newRelatedIds) =
      case newRelatedIds of
        [] -> op  -- no new related operations to add
        l -> case _operation_relatedOperations op of
          Nothing -> op  -- no previous related operations
            { _operation_relatedOperations = justSortRelated l }
          Just oldRelatedIds -> op
            { _operation_relatedOperations = justSortRelated $! (oldRelatedIds <> l) }

operation
    :: ChainwebOperationStatus
    -> OperationType
    -> AccountLog
    -> Word64
    -> [OperationId]
    -> Operation
operation ostatus otype acctLog idx related =
  Operation
    { _operation_operationId = OperationId idx Nothing
    , _operation_relatedOperations = someRelatedOps
    , _operation_type = sshow otype
    , _operation_status = sshow ostatus
    , _operation_account = Just $ accountId (_accountLogKey acctLog)
    , _operation_amount = Just $ kdaToRosettaAmount $
                          _balanceDelta $ _accountLogBalanceDelta acctLog
    , _operation_coinChange = Nothing
    , _operation_metadata = opMeta
    }
  where
    someRelatedOps = case related of
      [] -> Nothing
      li -> Just li
    opMeta = Just $ toObject $ OperationMetaData
      { _operationMetaData_prevOwnership = _accountLogPrevGuard acctLog
      , _operationMetaData_currOwnership = _accountLogCurrGuard acctLog
      }

parseOp
    :: Operation
    -> Either RosettaError (AccountId, P.ParsedDecimal, P.KeySet)
parseOp (Operation i _ typ stat someAcct someAmt _ someMeta) = do
  typ @?= "TransferOrCreateAcct"
  stat @?= "Successful"
  acct <- someAcct @?? "Missing AccountId"
  amtDelta <- someAmt @?? "Missing Amount" >>= parseAmount
  (OperationMetaData prevOwn currOwn) <- someMeta @?? "Missing metadata"
                                             >>= extractMetaData
  prevOwn @?= currOwn   -- ensure that the ownership wasn't rotated
  ownership <- hushResult (fromJSON currOwn) @??
               "Only Pact KeySet is supported for account ownership"

  pure (acct, P.ParsedDecimal amtDelta, ownership)

  where
    (@??) :: Maybe a -> String -> Either RosettaError a
    Nothing @?? msg =
      Left $ stringRosettaError RosettaInvalidOperation $
        "Operation id=" ++ show i ++ ": " ++ msg
    (Just a) @?? _ = pure a

    (@?=)
        :: (Show a, Eq a) => a
        -> a
        -> Either RosettaError ()
    actual @?= expected
      | actual == expected = pure ()
      | otherwise =
        Left $ stringRosettaError RosettaInvalidOperation $
          "Operation id=" ++ show i ++ ": expected " ++ show expected
          ++ " but received " ++ show actual


-- | Timestamp of the block in milliseconds since the Unix Epoch.
-- NOTE: Chainweb provides this timestamp in microseconds.
rosettaTimestamp :: BlockHeader -> Word64
rosettaTimestamp bh = BA.unLE . BA.toLE $ fromInteger msTime
  where
    msTime = int $ microTime `div` ms
    TimeSpan ms = millisecond
    microTime = encodeTimeToWord64 $ _bct (_blockCreationTime bh)


-- | How to convert from atomic units to standard units in Rosetta Currency.
defaultNumOfDecimals :: Word
defaultNumOfDecimals = 12

defaultCurrency :: Currency
defaultCurrency = Currency "KDA" defaultNumOfDecimals Nothing

kdaToRosettaAmount :: Decimal -> Amount
kdaToRosettaAmount k = Amount (sshow amount) defaultCurrency Nothing
  where
    -- Value in atomic units represented as an arbitrary-sized signed integer.
    amount :: Integer
    amount = floor $ k * realToFrac ((10 :: Integer) ^ defaultNumOfDecimals)

parseAmount :: Amount -> Either RosettaError Decimal
parseAmount a@(Amount txt (Currency _ numDecs _) _) = do
  validateCurrency (_amount_currency a)
  P.ParsedInteger i <- f $ String txt
  pure $ Decimal (fromIntegral numDecs) i
  where
    f = toRosettaError RosettaInvalidAmount .  noteResult . fromJSON

validateCurrency :: Currency -> Either RosettaError ()
validateCurrency curr
  | curr /= defaultCurrency =
    Left $ stringRosettaError RosettaInvalidAmount $
      "Expected currency " ++ show defaultCurrency ++
      " but received currency " ++ show curr
  | otherwise = pure ()

--------------------------------------------------------------------------------
-- Rosetta Exceptions --
--------------------------------------------------------------------------------

data RosettaFailure
    = RosettaChainUnspecified
    | RosettaInvalidChain
    | RosettaMempoolBadTx
    | RosettaUnparsableTx
    | RosettaInvalidTx
    | RosettaInvalidBlockchainName
    | RosettaMismatchNetworkName
    | RosettaPactExceptionThrown
    | RosettaExpectedBalDecimal
    | RosettaInvalidResultMetaData
    | RosettaSubAcctUnsupported
    | RosettaMismatchTxLogs
    | RosettaUnparsableTxLog
    | RosettaInvalidBlockHeight
    | RosettaBlockHashNotFound
    | RosettaUnparsableBlockHash
    | RosettaOrphanBlockHash
    | RosettaMismatchBlockHashHeight
    | RosettaPayloadNotFound
    | RosettaUnparsableTxOut
    | RosettaTxIdNotFound
    | RosettaUnparsableTransactionId
    | RosettaInvalidAccountKey
    | RosettaUnparsableMetaData
    | RosettaMissingMetaData
    | RosettaMissingPublicKeys
    | RosettaMissingExpectedPublicKey
    | RosettaInvalidAmount
    | RosettaInvalidOperation
    | RosettaInvalidOperations
    | RosettaInvalidPublicKey
    | RosettaInvalidSignature
    | RosettaInvalidAccountProvided
    | RosettaInvalidKAccount
    deriving (Show, Enum, Bounded, Eq)


-- TODO: Better grouping of rosetta error index?
rosettaError :: RosettaFailure -> Maybe Object -> RosettaError
rosettaError RosettaChainUnspecified = RosettaError 0 "No SubNetwork (chain) specified" False
rosettaError RosettaInvalidChain = RosettaError 1 "Invalid SubNetwork (chain) value" False
rosettaError RosettaMempoolBadTx = RosettaError 2 "Transaction not present in mempool" False
rosettaError RosettaUnparsableTx = RosettaError 3 "Transaction not parsable" False
rosettaError RosettaInvalidTx = RosettaError 4 "Invalid transaction" False
rosettaError RosettaInvalidBlockchainName = RosettaError 5 "Invalid blockchain name" False
rosettaError RosettaMismatchNetworkName = RosettaError 6 "Invalid Chainweb network name" False
rosettaError RosettaPactExceptionThrown =
  RosettaError 7 "A pact exception was thrown" False  -- TODO if retry could succeed
rosettaError RosettaExpectedBalDecimal = RosettaError 8 "Expected balance as a decimal" False
rosettaError RosettaInvalidResultMetaData = RosettaError 9 "Invalid meta data field in command result" False
rosettaError RosettaSubAcctUnsupported = RosettaError 10 "Sub account identifier is not supported" False
rosettaError RosettaMismatchTxLogs =
  RosettaError 11 "Unable to match transactions to transaction logs as expected" False
rosettaError RosettaUnparsableTxLog = RosettaError 12 "TxLogs not parsable" False
rosettaError RosettaInvalidBlockHeight = RosettaError 13 "Invalid block height" False -- TODO if retry could succeed
rosettaError RosettaBlockHashNotFound = RosettaError 14 "Block hash was not found" False
rosettaError RosettaUnparsableBlockHash = RosettaError 15 "Block hash not parsable" False
rosettaError RosettaOrphanBlockHash = RosettaError 16 "Block hash not in the latest fork" False
rosettaError RosettaMismatchBlockHashHeight = RosettaError 17 "Block hash and block height did not match" False
rosettaError RosettaPayloadNotFound = RosettaError 18 "Block payload not found" False
rosettaError RosettaUnparsableTxOut = RosettaError 19 "Transaction output not parsable" False
rosettaError RosettaTxIdNotFound = RosettaError 20 "Transaction Id not found in block" False
rosettaError RosettaUnparsableTransactionId = RosettaError 21 "Transaction Id not parsable" False
rosettaError RosettaInvalidAccountKey = RosettaError 22 "Invalid AccountId address" False
rosettaError RosettaUnparsableMetaData = RosettaError 24 "Unparsable metadata field" False
rosettaError RosettaMissingMetaData = RosettaError 25 "Required metadata field is missing" False
rosettaError RosettaMissingPublicKeys = RosettaError 26 "Required public_keys field is missing" False
rosettaError RosettaMissingExpectedPublicKey = RosettaError 27 "Expected public key not provided" False
rosettaError RosettaInvalidAmount = RosettaError 28 "Invalid Amount type" False
rosettaError RosettaInvalidOperation = RosettaError 29 "Invalid Operation type" False
rosettaError RosettaInvalidOperations = RosettaError 30 "Invalid Operations list found" False
rosettaError RosettaInvalidPublicKey = RosettaError 31 "Invalid PublicKey" False
rosettaError RosettaInvalidSignature = RosettaError 32 "Invalid Signature" False
rosettaError RosettaInvalidAccountProvided = RosettaError 33 "Invalid Account was provided" False
rosettaError RosettaInvalidKAccount = RosettaError 34 "Invalid k:Account" False

rosettaError' :: RosettaFailure -> RosettaError
rosettaError' f = rosettaError f Nothing

stringRosettaError :: RosettaFailure -> String -> RosettaError
stringRosettaError e msg = rosettaError e $ Just $
  HM.fromList ["error_message" .= msg ]

--------------------------------------------------------------------------------
-- Misc Helper Functions --
--------------------------------------------------------------------------------

maybePair :: (ToJSON a) => T.Text -> Maybe a -> (T.Text, Maybe Value)
maybePair name Nothing = (name, Nothing)
maybePair name (Just v) = (name, Just (toJSON v))

toPairOmitMaybe :: [Pair] -> [(T.Text, Maybe Value)] -> [Pair]
toPairOmitMaybe defPairs li = allPairs
  where
    allPairs = foldl' f defPairs li
    f acc (_, Nothing) = acc
    f acc (t, Just p) = acc ++ [t .= p]

toJSONOmitMaybe :: [Pair] -> [(T.Text, Maybe Value)] -> Value
toJSONOmitMaybe defPairs li = object $ toPairOmitMaybe defPairs li

toRosettaError
    :: RosettaFailure
    -> Either String a
    -> Either RosettaError a
toRosettaError failure = annotate (stringRosettaError failure)


ksToPubKeys :: P.KeySet -> [T.Text]
ksToPubKeys (P.KeySet pkSet _) =
  map (T.decodeUtf8 . P._pubKey) (S.toList pkSet)


parsePubKeys :: T.Text -> Value -> Either RosettaError [T.Text]
parsePubKeys k v = do
  g :: (P.Guard PactValue) <- toRosettaError RosettaInvalidAccountProvided
    $ noteResult $ fromJSON v
  case g of
    P.GUser _ -> pure []
    P.GKeySet ks -> pure $ ksToPubKeys ks
    _ -> Left $ stringRosettaError RosettaInvalidAccountProvided $
         "Account=" ++ show k ++
         ": Rosetta only supports ownership of type UserGuard and KeySet"


extractMetaData :: (FromJSON a) => Object -> Either RosettaError a
extractMetaData = toRosettaError RosettaUnparsableMetaData
                  . noteResult . fromJSON . Object

-- | Guarantees that the `ChainId` given actually belongs to this
-- `ChainwebVersion`. This doesn't guarantee that the chain is active.
--
readChainIdText :: ChainwebVersion -> T.Text -> Maybe ChainId
readChainIdText v c = do
  cid <- readMaybe @Word (T.unpack c)
  mkChainId v maxBound cid

-- TODO: document
maxRosettaNodePeerLimit :: Natural
maxRosettaNodePeerLimit = 64

rowDataToAccountLog :: AccountRow -> Maybe AccountRow -> AccountLog
rowDataToAccountLog (currKey, currBal, currGuard) prev = do
  case prev of
    Nothing ->
      -- First time seeing account
      AccountLog
        { _accountLogKey = currKey
        , _accountLogBalanceDelta = BalanceDelta currBal
        , _accountLogCurrGuard = currGuard
        , _accountLogPrevGuard = currGuard
        }
    Just (_, prevBal, prevGuard) ->
      -- Already seen this account
      AccountLog
        { _accountLogKey = currKey
        , _accountLogBalanceDelta = BalanceDelta (currBal - prevBal)
        , _accountLogCurrGuard = currGuard
        , _accountLogPrevGuard = prevGuard
        }

-- | Parse TxLog Value into fungible asset account columns
txLogToAccountRow :: P.TxLog Value -> Maybe AccountRow
txLogToAccountRow (P.TxLog _ key obj) = do
  P.RowData _ (P.ObjectMap row) :: P.RowData <- (hushResult . fromJSON) obj
  guard :: Value <- toJSON . P.rowDataToPactValue <$> M.lookup "guard" row
  case M.lookup "balance" row of
    Just (P.RDLiteral (LDecimal bal)) -> pure (key, bal, guard)
    _ -> Nothing

hushResult :: Result a -> Maybe a
hushResult (Success w) = Just w
hushResult (Error _) = Nothing

noteResult :: Result a -> Either String a
noteResult (Success w) = Right w
noteResult (Error e) = Left e

annotate :: (a -> c) -> Either a b -> Either c b
annotate f (Left e) = Left $ f e
annotate _ (Right r) = Right r

overwriteError :: a -> Either b c -> Either a c
overwriteError e (Left _) = Left e
overwriteError _ (Right r) = Right r

noteOptional :: a -> Either a (Maybe c) -> Either a c
noteOptional e (Right Nothing) = Left e
noteOptional _ (Right (Just c)) = pure c
noteOptional _ (Left oe) = Left oe
