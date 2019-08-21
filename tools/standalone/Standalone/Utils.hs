{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Standalone.Utils where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Lens

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as SB
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector

-- pact imports

import Pact.ApiReq
import qualified Pact.Types.ChainId as PactChain
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.RPC
import Pact.Types.Util

-- chainweb imports

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.Types
import Chainweb.Transaction
import Chainweb.WebPactExecutionService
-- import Chainweb.WebPactExecutionService.Types
-- import qualified Chainweb.Mempool.InMem as Mempool
import qualified Chainweb.Mempool.InMemTypes as Mempool
import qualified Chainweb.Mempool.Mempool as Mempool
-- import qualified Chainweb.Pact.BloomCache as Bloom

testKeyPairs :: IO [SomeKeyPair]
testKeyPairs = do
    let (pub, priv, addr, scheme) = someED25519Pair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
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


defaultMemPoolAccess :: ChainId -> Int -> MemPoolAccess
defaultMemPoolAccess cid blocksize  = MemPoolAccess
    { mpaGetBlock = \height _hash _prevBlock ->
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
              cmd <- mkCommand ks pm nonce msg
              case verifyCommand cmd of
                ProcSucc t -> return $ fmap (k t) (SB.toShort <$> cmd)
                ProcFail e -> throwM $ userError e

          k t bs = PayloadWithText bs (_cmdPayload t)

    -- | Merge a list of JSON Objects together. Note: this will yield an empty
    -- object in the case that there are no objects in the list of values.
    --
    mergeObjects :: [Value] -> Value
    mergeObjects = Object . HM.unions . foldr unwrap' []
      where
        unwrap' (Object o) = (:) o
        unwrap' _ = id


mkPactExecutionService' :: TQueue RequestMsg -> PactExecutionService
mkPactExecutionService' q = emptyPactExecutionService
  { _pactValidateBlock = \h pd -> do
      mv <- validateBlock h pd q
      r <- takeMVar mv
      case r of
          (Right !pdo) -> return pdo
          Left e -> throwM e
  , _pactNewBlock = \m h -> do
      mv <- newBlock m h q
      r <- takeMVar mv
      case r of
          (Right !pdo) -> return pdo
          Left e -> throwM e
  }

-- TODO: The type InMempoolConfig contains parameters that should be
-- configurable as well as parameters that are determined by the chainweb
-- version or the chainweb protocol. These should be separated in to two
-- different types.
--
mempoolConfig :: Bool -> Mempool.InMemConfig ChainwebTransaction
mempoolConfig enableReIntro = Mempool.InMemConfig
    Mempool.chainwebTransactionConfig
    blockGasLimit
    mempoolReapInterval
    maxRecentLog
    enableReIntro
  where
    blockGasLimit = 100000               -- TODO: policy decision
    mempoolReapInterval = 60 * 20 * 1000000   -- 20 mins
    maxRecentLog = 2048                   -- store 2k recent transaction hashes
