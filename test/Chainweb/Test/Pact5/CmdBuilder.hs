{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Chainweb.Test.Pact5.CmdBuilder where

import Control.Lens hiding ((.=))
import Pact.Core.Command.Types
import Data.Text (Text)
import GHC.Generics
import Pact.Core.Capabilities
import Data.Aeson (Key)
import Data.Aeson.Types (Value)
import Pact.Core.Guards
import Pact.Core.Verifiers (Verifier, ParsedVerifierProof)
import Pact.Core.Command.RPC
import Pact.Core.ChainData
import Pact.Core.Gas.Types
import Control.Exception.Safe
import Control.Monad.IO.Class
import Chainweb.Version
import qualified Chainweb.ChainId
import qualified Chainweb.ChainId as Chainweb
import Data.ByteString (ByteString)
import qualified Chainweb.Pact5.Transaction as Pact5
import qualified Data.Text.Encoding as T
import Chainweb.Utils
import GHC.Exts (IsString(..))
import Data.Maybe
import Pact.Core.Command.Crypto
import Pact.Core.Command.Util
import qualified Data.Text as T
import Pact.Core.Names (QualifiedName, DefPactId)
import Pact.Core.PactValue
import Data.Aeson
import qualified Data.Set as Set
import Pact.Core.StableEncoding
import Chainweb.Pact.RestAPI.Server (validatePact5Command)
import Pact.Core.Command.Client (ApiKeyPair (..), mkCommandWithDynKeys)
import Pact.JSON.Legacy.Value (toLegacyJson)
import System.Random
import Control.Monad

type TextKeyPair = (Text,Text)

sender00 :: TextKeyPair
sender00 = ("368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
           ,"251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898")

sender01 :: TextKeyPair
sender01 = ("6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
           ,"2beae45b29e850e6b1882ae245b0bab7d0689ebdd0cd777d4314d24d7024b4f7")

sender02WebAuthnPrefixed :: TextKeyPair
sender02WebAuthnPrefixed =
           ("WEBAUTHN-a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf"
           ,"fecd4feb1243d715d095e24713875ca76c476f8672ec487be8e3bc110dd329ab")

sender02WebAuthn :: TextKeyPair
sender02WebAuthn =
           ("a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf"
           ,"fecd4feb1243d715d095e24713875ca76c476f8672ec487be8e3bc110dd329ab")

sender03WebAuthn :: TextKeyPair
sender03WebAuthn =
           ("a4010103272006215820ad72392508272b4c45536976474cdd434e772bfd630738ee9aac7343e7222eb6"
           ,"ebe7d1119a53863fa64be7347d82d9fcc9ebeb8cbbe480f5e8642c5c36831434")

allocation00KeyPair :: TextKeyPair
allocation00KeyPair =
    ( "d82d0dcde9825505d86afb6dcc10411d6b67a429a79e21bda4bb119bf28ab871"
    , "c63cd081b64ae9a7f8296f11c34ae08ba8e1f8c84df6209e5dee44fa04bcb9f5"
    )


-- | Make trivial keyset data
mkKeySetData :: Key -> [TextKeyPair] -> Value
mkKeySetData name keys = object [ name .= map fst keys ]

sender00Ks :: KeySet
sender00Ks = KeySet
    (Set.fromList [PublicKeyText "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"])
    KeysAll


-- | Pair a 'Signer' with private key.
data CmdSigner = CmdSigner
  { _csSigner :: !(Signer QualifiedName PactValue)
  , _csPrivKey :: !Text
  } deriving (Eq,Show,Ord,Generic)
makeLenses ''CmdSigner

-- | Make ED25519 signer.
mkEd25519Signer :: Text -> Text -> [CapToken QualifiedName PactValue] -> CmdSigner
mkEd25519Signer pubKey privKey caps = CmdSigner
  { _csSigner = signer
  , _csPrivKey = privKey
  }
  where
    signer = Signer
      { _siScheme = Nothing
      , _siPubKey = pubKey
      , _siAddress = Nothing
      , _siCapList = caps }

mkEd25519Signer' :: TextKeyPair -> [CapToken QualifiedName PactValue] -> CmdSigner
mkEd25519Signer' (pub,priv) = mkEd25519Signer pub priv

mkWebAuthnSigner :: Text -> Text -> [CapToken QualifiedName PactValue] -> CmdSigner
mkWebAuthnSigner pubKey privKey caps = CmdSigner
  { _csSigner = signer
  , _csPrivKey = privKey
  }
  where
    signer = Signer
      { _siScheme = Just WebAuthn
      , _siPubKey = pubKey
      , _siAddress = Nothing
      , _siCapList = caps }

mkWebAuthnSigner' :: TextKeyPair -> [CapToken QualifiedName PactValue] -> CmdSigner
mkWebAuthnSigner' (pub, priv) caps = mkWebAuthnSigner pub priv caps

-- | Chainweb-oriented command builder.
data CmdBuilder = CmdBuilder
  { _cbSigners :: ![CmdSigner]
  , _cbVerifiers :: ![Verifier ParsedVerifierProof]
  , _cbRPC :: !(PactRPC Text)
  , _cbNonce :: !(Maybe Text)
  , _cbChainId :: !Chainweb.ChainId
  , _cbSender :: !Text
  , _cbGasLimit :: !GasLimit
  , _cbGasPrice :: !GasPrice
  , _cbTTL :: !TTLSeconds
  , _cbCreationTime :: !TxCreationTime
  } deriving (Eq,Show,Generic)
makeLenses ''CmdBuilder

-- | Make code-only Exec PactRPC
mkExec' :: Text -> PactRPC Text
mkExec' ecode = mkExec ecode (PObject mempty)

-- | Make Exec PactRPC
mkExec :: Text -> PactValue -> PactRPC Text
mkExec ecode edata = Exec $ ExecMsg ecode edata

mkCont :: ContMsg -> PactRPC Text
mkCont = Continuation

mkContMsg :: DefPactId -> Int -> ContMsg
mkContMsg pid step = ContMsg
  { _cmPactId = pid
  , _cmStep = step
  , _cmRollback = False
  , _cmData = PObject mempty
  , _cmProof = Nothing }

-- | Default builder.
defaultCmd :: CmdBuilder
defaultCmd = CmdBuilder
  { _cbSigners = []
  , _cbVerifiers = []
  , _cbRPC = mkExec' "1"
  , _cbNonce = Nothing
  , _cbChainId = unsafeChainId 0
  , _cbSender = "sender00"
  , _cbGasLimit = GasLimit (Gas 10_000)
  , _cbGasPrice = GasPrice 0.000_1
  , _cbTTL = TTLSeconds 300 -- 5 minutes
  , _cbCreationTime = TxCreationTime 0 -- epoch
  }

-- | Build parsed + verified Pact command

-- TODO: Use the new `assertPact4Command` function.
buildCwCmd :: (MonadThrow m, MonadIO m) => ChainwebVersion -> CmdBuilder -> m Pact5.Transaction
buildCwCmd v cmd = buildTextCmd v cmd >>= \(c :: Command Text) ->
  case validatePact5Command v (_cbChainId cmd) c of
    Left err -> throwM $ userError $ "buildCmd failed: " ++ err
    Right cmd' -> return cmd'

-- | Build unparsed, unverified command
--
buildTextCmd :: (MonadThrow m, MonadIO m) => ChainwebVersion -> CmdBuilder -> m (Command Text)
buildTextCmd v = fmap (fmap T.decodeUtf8) . buildRawCmd v

-- | Build a raw bytestring command
--
buildRawCmd :: (MonadThrow m, MonadIO m) => ChainwebVersion -> CmdBuilder -> m (Command ByteString)
buildRawCmd v CmdBuilder{..} = do
    kps <- liftIO $ traverse mkDynKeyPairs _cbSigners
    nonce <- liftIO $ maybe (fmap T.pack $ replicateM 10 $ randomRIO ('a', 'z')) return _cbNonce
    cmd <- liftIO $ mkCommandWithDynKeys kps _cbVerifiers (StableEncoding pm) nonce (Just nid) _cbRPC
    pure cmd
  where
    nid = NetworkId (sshow v)
    cid = ChainId $ sshow (chainIdInt _cbChainId :: Int)
    pm = PublicMeta cid _cbSender _cbGasLimit _cbGasPrice _cbTTL _cbCreationTime

dieL :: MonadThrow m => [Char] -> Either [Char] a -> m a
dieL msg = either (\s -> throwM $ userError $ msg ++ ": " ++ s) return

mkDynKeyPairs :: MonadThrow m => CmdSigner -> m (DynKeyPair, [CapToken QualifiedName PactValue])
mkDynKeyPairs (CmdSigner Signer{..} privKey) =
  case (fromMaybe ED25519 _siScheme, _siPubKey, privKey) of
    (ED25519, pub, priv) -> do
      pub' <- either diePubKey return $ parseEd25519PubKey =<< parseB16TextOnly pub
      priv' <- either diePrivKey return $ parseEd25519SecretKey =<< parseB16TextOnly priv
      return $ (DynEd25519KeyPair (pub', priv'), _siCapList)

    (WebAuthn, pub, priv) -> do
      let (pubKeyStripped, wasPrefixed) = fromMaybe
            (pub, WebAuthnPubKeyBare)
            ((,WebAuthnPubKeyPrefixed) <$> T.stripPrefix webAuthnPrefix pub)
      pubWebAuthn <-
        either diePubKey return (parseWebAuthnPublicKey =<< parseB16TextOnly pubKeyStripped)
      privWebAuthn <-
        either diePrivKey return (parseWebAuthnPrivateKey =<< parseB16TextOnly priv)
      return $ (DynWebAuthnKeyPair wasPrefixed pubWebAuthn privWebAuthn, _siCapList)
  where
    diePubKey str = error $ "pubkey: " <> str
    diePrivKey str = error $ "privkey: " <> str

toApiKp :: MonadThrow m => CmdSigner -> m ApiKeyPair
toApiKp (CmdSigner Signer{..} privKey) = do
  sk <- dieL "private key" $ parseB16TextOnly privKey
  pk <- dieL "public key" $ parseB16TextOnly _siPubKey
  let keyPair = ApiKeyPair (PrivBS sk) (Just (PubBS pk)) _siAddress _siScheme (Just _siCapList)
  return $! keyPair
