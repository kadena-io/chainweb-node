{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.Pact.SPV.Hyperlane where

import Control.Error
import Control.Lens hiding (index)
import Control.Monad (when, unless)
import Control.Monad.Catch
import Control.Monad.Except

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as BS
import Data.DoubleWord
import Data.Decimal
import Data.Default (def)
import Data.Foldable (foldl')
import Data.Traversable (forM)
import Data.Ratio
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V

import qualified Crypto.Secp256k1 as ECDSA

import Ethereum.Misc hiding (Word256)

import Pact.Types.Runtime

import Chainweb.Pact.SPV.Hyperlane.Binary
import Chainweb.Utils.Serialization (putRawByteString, runPutS, runGetS, putWord32be)

-- | Parses the object and evaluates Hyperlane command
evalHyperlaneCommand :: Object Name -> ExceptT Text IO (Object Name)
evalHyperlaneCommand (_objectMap . _oObject -> om)
  | Just (TLitString storageLocation) <- M.lookup "storageLocation" om
  , Just (TLitString sig) <- M.lookup "signature" om
  = recoverAddressValidatorAnnouncement storageLocation sig

  -- process
  | Just (TLitString message) <- M.lookup "message" om
  , Just (TLitString metadata) <- M.lookup "metadata" om
  , Just (TList validators _ _) <- M.lookup "validators" om
  , Just (TLitInteger threshold) <- M.lookup "threshold" om
  = do
    parsedValidators <- forM validators $ \case
      (TLitString v) -> pure v
      _ -> throwError $ Text.pack $ "Only string validators are supported"
    verifySignatures message metadata parsedValidators (fromInteger threshold)

  -- dispatch
  | Just (TObject o _) <- M.lookup "message" om
  = encodeHyperlaneMessage o

  | otherwise = throwError "Unknown hyperlane command"

-- | Decodes Hyperlane binary message and metadata,
-- verifies against the provided signatures using the provided threshold.
verifySignatures :: Text -> Text -> V.Vector Text -> Int -> ExceptT Text IO (Object Name)
verifySignatures hexMessage hexMetadata validators threshold = do
  message <- case decodeHex hexMessage of
    Right s -> pure s
    Left e -> throwError $ Text.pack $ "Decoding of HyperlaneMessage failed: " ++ e

  HyperlaneMessage{..} <- runGetS getHyperlaneMessage message

  metadata <- case decodeHex hexMetadata of
    Right s -> pure s
    Left e -> throwError $ Text.pack $ "Decoding of Metadata failed: " ++ e

  MessageIdMultisigIsmMetadata{..} <- runGetS getMessageIdMultisigIsmMetadata metadata

  let
    domainHash = getKeccak256Hash $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putWord32be hmOriginDomain
      putRawByteString mmimOriginMerkleTreeAddress
      putRawByteString "HYPERLANE"

  let messageId = getKeccak256Hash message

  let
    hash' = getKeccak256Hash $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString domainHash
      putRawByteString mmimSignedCheckpointRoot
      putWord32be mmimSignedCheckpointIndex
      putRawByteString messageId
  let
    digest = keccak256 $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString ethereumHeader
      putRawByteString hash'

  addresses <- catMaybes <$> mapM (recoverAddress digest) mmimSignatures

  when (length addresses < threshold) $
    throwError $ Text.pack $ "The number of recovered addresses from the signatures is less than threshold: " ++ show threshold

  binaryValidators <- forM validators $ \val -> case decodeHex val of
    Right v -> pure v
    Left e -> throwError $ "Failed to decode a validator (" <> val <> "):" <> Text.pack e

  -- Requires that m-of-n validators verify a merkle root, and verifies a merkle proof of message against that root.
  --
  -- The signature addresses and validator addresses should be in the same order.
  --
  -- The original algorithm in hyperlane.
  -- https://github.com/hyperlane-xyz/hyperlane-monorepo/blob/v3/solidity/contracts/isms/multisig/AbstractMultisigIsm.sol#L67
  let verificationAddresses = take threshold addresses
  let verifyStep (_, vals) signer = case V.elemIndex signer vals of
        Just i -> let newV = V.drop (i + 1) vals in (True, newV)
        Nothing -> (False, V.empty)
  let verified = fst $ foldl' verifyStep (False, binaryValidators) verificationAddresses

  unless verified $ throwError "Verification failed"

  let TokenMessageERC20{..} = hmTokenMessage
  let
    encodedSender = encodeHex hmSender
    hmObj = obj
          [ ("version", tLit $ LInteger $ toInteger hmVersion)
          , ("nonce", tLit $ LInteger $ toInteger hmNonce)
          , ("originDomain", tLit $ LInteger $ toInteger hmOriginDomain)
          , ("sender", tStr $ asString encodedSender)
          , ("destinationDomain", tLit $ LInteger $ toInteger hmDestinationDomain)
          , ("recipient", tStr $ asString hmRecipient)
          , ("tokenMessage", obj
              ([ ("recipient", tStr $ asString tmRecipient)
               , ("amount", tLit $ LDecimal $ wordToDecimal tmAmount)
              ])
            )
          ]
  pure $ mkObject [ ("message", hmObj), ("messageId", tStr $ asString $ encodeHex messageId) ]

-- | Recovers address from provided signature using the calculated digest with provided storageLocation.
recoverAddressValidatorAnnouncement :: Text -> Text -> ExceptT Text IO (Object Name)
recoverAddressValidatorAnnouncement storageLocation sig = do
  signatureBinary <- case decodeHex sig of
          Right s -> pure s
          Left e -> throwError $ Text.pack $ "Decoding of signature failed: " ++ e

  let
    -- This is a kadena's domain hash calculated in Solidity as
    -- keccak256(abi.encodePacked(626, "kb-mailbox", "HYPERLANE_ANNOUNCEMENT"))
    domainHashHex :: Text
    domainHashHex = "0xa69e6ef1a8e62aa6b513bd7d694c6d237164fb04df4e5fb4106e47bf5b5a0428"

  domainHash <- case decodeHex domainHashHex of
          Right s -> pure s
          Left e -> throwError $ Text.pack $ "Decoding of domainHashHex failed: " ++ e

  let
    hash' = getKeccak256Hash $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString domainHash
      putRawByteString $ Text.encodeUtf8 storageLocation

  let
    announcementDigest = keccak256 $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString ethereumHeader
      putRawByteString hash'

  address <- recoverAddress announcementDigest signatureBinary
  let addr = tStr . asString . encodeHex <$> address

  case addr of
    Just a -> return $ mkObject [ ("address", a) ]
    Nothing -> throwError "Failed to recover address"

-- | Encodes pact object into Hyperlane binary message
encodeHyperlaneMessage :: Object Name -> ExceptT Text IO (Object Name)
encodeHyperlaneMessage o = do
  let
    om = _objectMap $ _oObject o
    tokenMessage = om ^? at "tokenMessage" . _Just . _TObject . _1

  hmTokenMessage <- case parseTokenMessageERC20 =<< tokenMessage of
    Just t -> pure t
    _ -> throwError "Couldn't encode TokenMessageERC20"

  let
    newObj = do
      hmVersion <- om ^? at "version" . _Just . _TLiteral . _1 . _LInteger . to fromIntegral
      hmNonce <- om ^? at "nonce" . _Just . _TLiteral . _1 . _LInteger . to fromIntegral
      hmOriginDomain <- om ^? at "originDomain" . _Just . _TLiteral . _1 . _LInteger . to fromIntegral
      hmSender <- om ^? at "sender" . _Just . _TLiteral . _1 . _LString . to Text.encodeUtf8
      hmDestinationDomain <- om ^? at "destinationDomain" . _Just . _TLiteral . _1 . _LInteger . to fromIntegral
      hmRecipient <- om ^? at "recipient" . _Just . _TLiteral . _1 . _LString . to decodeHex . _Right

      let hm = HyperlaneMessage{..}
      let binaryHm = runPutS $ putHyperlaneMessage hm
      let messageId = encodeHex $ getKeccak256Hash binaryHm
      let hexHm = encodeHex binaryHm
      pure $ mkObject [ ("encodedMessage", tStr $ asString hexHm), ("messageId", tStr $ asString messageId) ]
  case newObj of
    Just o' -> pure o'
    _ -> throwError "Couldn't encode HyperlaneMessage"

-- | Parses 'TokenMessageERC20' from provided pact object.
parseTokenMessageERC20 :: Object Name -> Maybe TokenMessageERC20
parseTokenMessageERC20 o = do
  let om = _objectMap $ _oObject o
  tmRecipient <- om ^? at "recipient" . _Just . _TLiteral . _1 . _LString
  tmAmount <- om ^? at "amount" . _Just . _TLiteral . _1 . _LDecimal . to decimalToWord
  pure $ TokenMessageERC20{..}

encodeTokenMessageERC20 :: Object Name -> Maybe Text
encodeTokenMessageERC20 o = do
  tm <- parseTokenMessageERC20 o
  let hex = encodeHex $ runPutS $ putTokenMessageERC20 tm
  pure hex

-- | Recovers the address from keccak256 encoded digest and signature.
recoverAddress :: MonadThrow m => Keccak256Hash -> B.ByteString -> m (Maybe B.ByteString)
recoverAddress digest sig' = do
  fnDigest <- ECDSA.ecdsaMessageDigest $ _getBytesN $ _getKeccak256Hash digest
  let
    mkR s = ECDSA.ecdsaR $ BS.toShort s
    mkS s = ECDSA.ecdsaS $ BS.toShort s
    mkV s = ECDSA.ecdsaV $ BS.toShort s
    ecrecover sig = do
      -- signature is a 65 bytes long sequence (r, s, v), where r and s are both 32 bytes and v is 1 byte
      let (binR, sAndV) = B.splitAt 32 sig
      r <- mkR binR
      s <- mkS (B.take 32 sAndV)
      v <- mkV (B.drop 32 sAndV)
      pure $ ECDSA.ecdsaRecoverPublicKey fnDigest r s v <&> getAddress

  addr <- ecrecover sig'
  pure addr

-- | Returns an address, a rightmost 160 bits (20 bytes) of the keccak hash of the public key.
getAddress :: ECDSA.EcdsaPublicKey -> B.ByteString
getAddress pubkey = B.takeEnd ethereumAddressSize
    $ getKeccak256Hash
    $ BS.fromShort
    $ BS.drop 1 -- drop the first 0x04 byte the indicates that the key is encoded in compressed format
    $ ECDSA.ecdsaPublicKeyBytes pubkey

encodeHex :: B.ByteString -> Text
encodeHex = ((<>) "0x") . Text.decodeUtf8 . B.toStrict . Builder.toLazyByteString . Builder.byteStringHex

decodeHex :: Text -> Either String B.ByteString
decodeHex s
  | Just h <- Text.stripPrefix "0x" s = B16.decode $ Text.encodeUtf8 h
  | otherwise = Left "decodeHex: does not start with 0x"

-- | Header of the 32 bytes ethereum binary message.
ethereumHeader :: B.ByteString
ethereumHeader = "\x19" <> "Ethereum Signed Message:\n" <> "32"

decimalToWord :: Decimal -> Word256
decimalToWord d =
  let ethInWei = 1000000000000000000 -- 1e18
  in round $ d * ethInWei

wordToDecimal :: Word256 -> Decimal
wordToDecimal w =
  let ethInWei = 1000000000000000000 -- 1e18
  in fromRational (toInteger w % ethInWei)

getKeccak256Hash :: B.ByteString -> B.ByteString
getKeccak256Hash = BS.fromShort . _getBytesN . _getKeccak256Hash . keccak256

mkObject :: [(FieldKey, Term n)] -> Object n
mkObject ps = Object (ObjectMap (M.fromList ps)) TyAny Nothing def

obj :: [(FieldKey, Term n)] -> Term n
obj = toTObject TyAny def
