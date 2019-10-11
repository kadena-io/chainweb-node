{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Tools.Standalone.Utils where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Lens hiding ((.=))

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Default
import Data.FileEmbed
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as Vector
import qualified Data.Yaml as Y

import GHC.Generics

import System.Random

import Text.Printf

-- pact imports

import Pact.ApiReq
import qualified Pact.Types.ChainId as PactChain
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Gas
import Pact.Types.RPC
import Pact.Types.Util hiding (unwrap)

-- chainweb imports

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Pact.Backend.Types
import Chainweb.Time
import Chainweb.Transaction

testKeyPairs :: IO [SomeKeyPairCaps]
testKeyPairs = do
    let (pub, priv, addr, scheme) = someED25519Pair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme) Nothing
    mkKeyPairs [apiKP]


-- | note this is "sender00"'s key
someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
someED25519Pair =
    ( PubBS $ getByteString
        "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , PrivBS $ getByteString
        "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898"
    , "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , ED25519
    )

getByteString :: ByteString -> ByteString
getByteString = fst . B16.decode

formatB16PubKey :: SomeKeyPair -> Text
formatB16PubKey = toB16Text . formatPublicKey

onlyCoinTransferMemPoolAccess :: ChainId -> Int -> MemPoolAccess
onlyCoinTransferMemPoolAccess cid blocksize = MemPoolAccess
    { mpaGetBlock = \_ _ _ _ -> getTestBlock (chainIdToText cid) blocksize
    , mpaSetLastHeader = const $ return ()
    , mpaProcessFork = const $ return ()
    }

defaultMemPoolAccess :: ChainId -> Int -> MemPoolAccess
defaultMemPoolAccess cid blocksize  = MemPoolAccess
    { mpaGetBlock = \_preblockcheck height _hash _prevBlock ->
        makeBlock height cid blocksize ("(+ 1 2)", Nothing)
    , mpaSetLastHeader = const $ return ()
    , mpaProcessFork = const $ return ()
    }
  where
    makeBlock
        :: BlockHeight
        -> ChainId
        -> Int
        -> (Text, Maybe Value)
        -> IO (Vector.Vector ChainwebTransaction)
    makeBlock height cidd n = Vector.replicateM n . go
        where
          go (c, d) = do
              let dd = mergeObjects (toList d)
                  pm = def
                    & set pmSender "sender00"
                    & set pmGasLimit 100
                    & set pmGasPrice 0.1
                    & set pmChainId (PactChain.ChainId (chainIdToText cidd))
                  msg = Exec (ExecMsg c dd)
                  -- TODO: This might need to be something more fleshed out.
                  nonce = T.pack $ show height
              ks <- testKeyPairs
              cmd <- mkCommand ks pm nonce Nothing msg
              case verifyCommand cmd of
                ProcSucc t -> return $ mkPayloadWithText <$> t
                ProcFail e -> throwM $ userError e

data StopState
  = BlockStopCondition BlockStopState
  | TimeLength Int -- this is in terms of microseconds
  | Forever
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BlockStopState
  = Height BlockHeight
  | Weight BlockWeight
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

stopAtCutWeight :: BlockWeight -> CutDb cas -> IO ()
stopAtCutWeight bw db = do
    atomically $ do
        c <- _cutStm db
        unless (_cutWeight c >= bw) retry
    throwM $ userError msg
  where
    msg =
      "We have reached or passed "
      ++ (show bw)
      ++ ". Stopping chainweb-node!"

stopAtCutHeight :: BlockHeight -> CutDb cas -> IO ()
stopAtCutHeight bh db = do
    atomically $ do
        c <- _cutStm db
        unless (getAny $ foldMap go $ _cutMap c) retry
    throwM $ userError msg
  where
    go v = Any $ _blockHeight v >= bh
    msg =
      "We have reached or passed "
      ++ (show bh)
      ++ ". Stopping chainweb-node!"

getTestBlock
    :: Text
    -- ^ chain id
    -> Int
    -- ^ number of transactions in a block
    -> IO (Vector.Vector ChainwebTransaction)
getTestBlock cid blocksize = do
    let gen = do
          (sendera, senderb) <- distinctSenders
          signer <- stockKey sendera
          kpa <- mkKeyPairs [signer]
          amount <- randomIO @Double
          let code =
                T.pack $ genCode (T.unpack sendera) (T.unpack senderb) amount
              data' = Just $ object []
          return (head kpa , (sendera, PactTransaction code data'))
    (signers, pacttxs) <- Vector.unzip <$> Vector.replicateM blocksize gen
    nonce <- T.pack . show @(Time Int) <$> getCurrentTimeIntegral
    mkExecTransactions (PactChain.ChainId cid) (toList signers) nonce 10000 0.00000000001 3600 0 pacttxs
  where
    genCode :: String -> String -> Double -> String
    genCode = printf "(coin.transfer  \"%s\" \"%s\" %f)"

data PactTransaction = PactTransaction
  { _pactCode :: Text
  , _pactData :: Maybe Value
  } deriving (Eq, Show)

-- Make pact 'ExecMsg' transactions specifying sender, chain id of the signer,
-- signer keys, nonce, gas rate, gas limit, and the transactions
-- (with data) to execute.
--
mkExecTransactions
    :: PactChain.ChainId
      -- ^ chain id of execution
    -> [SomeKeyPairCaps]
      -- ^ signer keys
    -> Text
      -- ^ nonce
    -> GasLimit
      -- ^ starting gas
    -> GasPrice
      -- ^ gas rate
    -> TTLSeconds
      -- ^ time in seconds until expiry (from offset)
    -> TxCreationTime
      -- ^ time in seconds until creation (from offset)
    -> Vector.Vector (Text, PactTransaction)
      -- ^ the pact transactions with data to run
    -> IO (Vector.Vector ChainwebTransaction)
mkExecTransactions cid ks nonce0 gas gasrate ttl ct txs = do
    nref <- newIORef (0 :: Int)
    traverse (go nref) txs
  where
    go nref (sender, PactTransaction c d) = do
      let dd = mergeObjects (toList d)
          pm = PublicMeta cid sender gas gasrate ttl ct
          msg = Exec (ExecMsg c dd)

      nn <- readIORef nref
      writeIORef nref $! succ nn
      let nonce = T.append nonce0 (T.pack $ show nn)
      cmd <- mkCommand ks pm nonce Nothing msg
      case verifyCommand cmd of
        ProcSucc t -> return $! mkPayloadWithText <$> t
        ProcFail e -> throwM $ userError e

-- | Merge a list of JSON Objects together. Note: this will yield an empty
-- object in the case that there are no objects in the list of values.
--
mergeObjects :: [Value] -> Value
mergeObjects = Object . HM.unions . foldr unwrap []
  where
    unwrap (Object o) = (:) o
    unwrap _ = id

distinctSenders :: IO (Text, Text)
distinctSenders = do
    a <- randomRIO (0, 9 :: Int)
    b <- fix $ \f -> do
        b <- randomRIO (0, 9 :: Int)
        if b == a then f
          else return b
    return $ (append a, append b)
  where
    append s = T.pack ("sender0" ++ show s)

mkKeyset :: Text -> [PublicKeyBS] -> Value
mkKeyset p ks = object
  [ "pred" .= p
  , "keys" .= ks
  ]

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/genesis/testnet/keys.yaml")

stockKey :: Text -> IO ApiKeyPair
stockKey s = do
    let Right (Y.Object o) = Y.decodeEither' stockKeyFile
        Just (Y.Object kp) = HM.lookup s o
        Just (String pub) = HM.lookup "public" kp
        Just (String priv) = HM.lookup "secret" kp
        mkKeyBS = decodeKey . encodeUtf8
    return $ ApiKeyPair (PrivBS $ mkKeyBS priv) (Just $ PubBS $ mkKeyBS pub) Nothing (Just ED25519) Nothing

decodeKey :: ByteString -> ByteString
decodeKey = fst . B16.decode
