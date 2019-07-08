{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- The fast "inner loop" of the mining process. Sits idle upon startup until
-- work is submitted.
--
-- == Purpose and Expectations ==
--
-- This tool is a mid-level, push-based, private, multicore CPU miner for
-- Chainweb. By this we mean:
--
--   * mid-level: the miner is aware of what `BlockHeader`s are, but does not
--     necessarily know how to construct them. It has no concept of `Cut`s or
--     the cut network used by Chainweb - it simply attempts `Nonce`s until a
--     suitable hash is found that matches the current `HashTarget`.
--
--   * push-based: Work is submitted to the miner in the form of a candidate
--     `BlockHeader`, in which the `Nonce` has been completely guessed. The
--     miner then automatically handles cancelling previous mining attempts,
--     hashing, cycling the `Nonce`, and caching the result. Note that we
--     maintain no "long connections", so work submissions yield an HTTP
--     response immediately, and the results of successful mining must be polled
--     manually.
--
--   * private: Work can be submitted (and results polled) by anyone. It is
--     therefore expected that a miner's HTTP interface is not exposed to the
--     open internet.
--     __It is the duty of the user (you) to ensure a correct configuration!__
--
--   * multicore: the miner uses 1 CPU core by default, but can use as many as
--     you indicate. GPU support will be added soon.
--
-- Since this miner is "mid-level", it is up to the user to submit valid
-- `BlockHeader`s derived from some suitable `Cut` (a full @chainweb-node@
-- handles this automatically).
--
-- When mining has completed for some given work, the process will sit idle. At
-- most, between 128 and 256 recently mined `BlockHeader`s per chain will be
-- kept in the cache. If the miner process exits, the cache is lost.
-- __Unpolled results thus cannot be recovered after a miner has been shutdown.__
--
-- == Usage ==
--
-- === submit/ endpoint ===
--
-- A POST call, expecting a single `BlockHeader` in its standard JSON format.
-- This will cancel any current mining. If you wish to mine on multiple chains
-- simultaneously, you can run multiple miner processes on different ports.
--
-- === poll/ endpoint ===
--
-- A GET call, given a `ChainId` and `BlockHeight` where we expect there to be a
-- result. /May/ return a single `BlockHeader`, if mining were successful there.
-- Failure indicates one of the following:
--
--   * New work cancelled the previous, thus no result was saved.
--   * Mining may still succeed before the work stales, but just hasn't yet.
--   * The given `ChainId` and `BlockHeight` were incorrect.
--

module Main ( main ) where

import BasePrelude hiding (app, option)

import Control.Concurrent.Async (async, race, wait)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Error.Util (note)
import Control.Scheduler (Comp(..), replicateWork, terminateWith, withScheduler)

import Crypto.Hash.Algorithms (SHA512t_256)
import Crypto.Hash.IO

import qualified Data.ByteArray as BA
import Data.Bytes.Put (runPutS)
import qualified Data.ByteString as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))

import Foreign.Marshal.Alloc (allocaBytes)

import qualified Network.Wai.Handler.Warp as W

import Options.Applicative

import Servant.API
import Servant.Server (Application, Server, serve)

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Difficulty (HashTarget, encodeHashTarget)
import Chainweb.RestAPI.Orphans ()
import Chainweb.Time (Micros, Time, encodeTimeToWord64, getCurrentTimeIntegral)
import Chainweb.Utils (int, runGet)
import Chainweb.Version

--------------------------------------------------------------------------------
-- Servant

type API = "submit" :> ReqBody '[JSON] BlockHeader :> Post '[JSON] ()
    :<|> "poll" :> Capture "chainid" ChainId
                :> Capture "blockheight" BlockHeight
                :> Get '[JSON] (Maybe BlockHeader)

server :: Env -> Server API
server e = liftIO . submit e :<|> (\cid h -> liftIO $ poll e cid h)

app :: Env -> Application
app = serve (Proxy :: Proxy API) . server

--------------------------------------------------------------------------------
-- CLI

type ResultMap = HashMap (T2 ChainId BlockHeight) BlockHeader

data Env = Env
    { work :: TMVar BlockHeader
    , results :: TVar ResultMap
    , cores :: Word16
    , version :: ChainwebVersion
    , port :: Int
    }

pEnv :: TMVar BlockHeader -> TVar ResultMap -> Parser Env
pEnv tbh thm = Env tbh thm <$> pCores <*> pVersion <*> pPort

pCores :: Parser Word16
pCores = option auto
    (long "cores" <> metavar "COUNT" <> value 1
     <> help "Number of CPU cores to use (default: 1)")

pVersion :: Parser ChainwebVersion
pVersion = option cver
    (long "version" <> metavar "VERSION"
     <> value Testnet01
     <> help "Chainweb Network Version (default: testnet01)")
  where
    cver :: ReadM ChainwebVersion
    cver = eitherReader $ \s ->
        note "Illegal ChainwebVersion" . chainwebVersionFromText $ T.pack s

pPort :: Parser Int
pPort = option auto
    (long "port" <> metavar "PORT" <> value 8081
     <> help "Port on which to run the miner (default: 8081)")

--------------------------------------------------------------------------------
-- Work

main :: IO ()
main = do
    env <- (opts <$> newEmptyTMVarIO <*> newTVarIO mempty) >>= execParser
    miner <- async $ mining env
    W.run (port env) $ app env
    wait miner
  where
    opts :: TMVar BlockHeader -> TVar ResultMap -> ParserInfo Env
    opts tbh thm = info (pEnv tbh thm <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

-- | Submit a new `BlockHeader` to mine (i.e. to determine a valid `Nonce`).
--
submit :: Env -> BlockHeader -> IO ()
submit (work -> w) bh = atomically $
    isEmptyTMVar w >>= bool (void $ swapTMVar w bh) (putTMVar w bh)

-- | For some `ChainId` and `BlockHeight`, have we mined a result?
--
poll :: Env -> ChainId -> BlockHeight -> IO (Maybe BlockHeader)
poll (results -> tm) cid h = HM.lookup (T2 cid h) <$> readTVarIO tm

-- | Cease mining until another `submit` call is made.
--
-- halt :: IO ()
-- halt = undefined

-- | A supervisor thread that listens for new work and supervises mining threads.
--
mining :: Env -> IO ()
mining e = do
    bh <- atomically . takeTMVar $ work e
    race newWork (go bh) >>= traverse_ miningSuccess
    mining e
  where
    -- | Wait for new work to come in from a `submit` call. `readTMVar` has the
    -- effect of "waiting patiently" and will not cook the CPU.
    --
    newWork :: IO ()
    newWork = void . atomically . readTMVar $ work e

    -- | If the `go` call won the `race`, this function saves the result of
    -- that successful mining. If `newWork` won the race instead, then the `go`
    -- call is automatically cancelled.
    --
    miningSuccess :: BlockHeader -> IO ()
    miningSuccess new = atomically $ modifyTVar' (results e) f
      where
        key = T2 (_blockChainId new) (_blockHeight new)
        f m = HM.insert key new . bool (prune m) m $ HM.size m < int cap

    -- | Reduce the size of the result cache by half if we've crossed the "cap".
    -- Clears old results out by `BlockCreationTime`.
    --
    prune :: ResultMap -> ResultMap
    prune = HM.fromList
        . snd
        . splitAt (int cap)
        . sortBy (compare `on` (_blockCreationTime . snd))
        . HM.toList

    -- | The maximum number of `BlockHeader`s to keep in the cache before
    -- pruning.
    --
    cap :: Natural
    cap = 256 * (order . _chainGraph $ version e)

    comp :: Comp
    comp = case cores e of
        1 -> Seq
        n -> ParN n

    -- This `head` should be safe, since `withScheduler` can only exit if it
    -- found some legal result.
    go :: BlockHeader -> IO BlockHeader
    go bh = fmap head . withScheduler comp $ \sch ->
        replicateWork (int $ cores e) sch $
            usePowHash (version e) mine bh >>= terminateWith sch

-- | Select a hashing algorithm.
--
usePowHash :: ChainwebVersion -> (forall a. HashAlgorithm a => Proxy a -> f) -> f
usePowHash Test{} f = f $ Proxy @SHA512t_256
usePowHash TimedConsensus{} f = f $ Proxy @SHA512t_256
usePowHash PowConsensus{} f = f $ Proxy @SHA512t_256
usePowHash TimedCPM{} f = f $ Proxy @SHA512t_256
usePowHash Testnet00{} f = f $ Proxy @SHA512t_256
usePowHash Testnet01{} f = f $ Proxy @SHA512t_256

-- | This Miner makes low-level assumptions about the chainweb protocol. It may
-- break if the protocol changes.
--
-- TODO: Check the chainweb version to make sure this function can handle the
-- respective version.
--
-- TODO: Remove the `Proxy`?
--
mine :: forall a. HashAlgorithm a => Proxy a -> BlockHeader -> IO BlockHeader
mine _ h = BA.withByteArray initialTargetBytes $ \trgPtr -> do
    !ctx <- hashMutableInit @a
    bytes <- BA.copy initialBytes $ \buf ->
        allocaBytes (powSize :: Int) $ \pow -> do

            -- inner mining loop
            --
            -- We do 100000 hashes before we update the creation time.
            --
            let go 100000 !n = do
                    -- update the block creation time
                    ct <- getCurrentTimeIntegral
                    injectTime ct buf
                    go 0 n

                go !i !n = do
                    -- Compute POW hash for the nonce
                    injectNonce n buf
                    hash ctx buf pow

                    -- check whether the nonce meets the target
                    fastCheckTarget trgPtr (castPtr pow) >>= \case
                        True -> return ()
                        False -> go (succ i) (succ n)

            -- Start inner mining loop
            go (0 :: Int) $ _blockNonce h

    -- On success: deserialize and return the new BlockHeader
    runGet decodeBlockHeaderWithoutHash bytes
  where
    initialBytes :: B.ByteString
    !initialBytes = runPutS $ encodeBlockHeaderWithoutHash h

    initialTargetBytes :: B.ByteString
    !initialTargetBytes = runPutS $ encodeHashTarget target

    bufSize :: Int
    !bufSize = B.length initialBytes

    target :: HashTarget
    !target = _blockTarget h

    powSize :: Int
    !powSize = int $ hashDigestSize @a undefined

    --  Compute POW hash
    hash :: MutableContext a -> Ptr Word8 -> Ptr Word8 -> IO ()
    hash ctx buf pow = do
        hashMutableReset ctx
        BA.withByteArray ctx $ \ctxPtr -> do
            hashInternalUpdate @a ctxPtr buf (int bufSize)
            hashInternalFinalize ctxPtr (castPtr pow)
    {-# INLINE hash #-}

    -- | `injectTime` and `injectNonce` make low-level assumptions about the
    -- byte layout of a hashed `BlockHeader`. If that layout changes, these
    -- functions need to be updated. The assumption allows us to iterate on new
    -- nonces quickly.
    --
    injectTime :: Time Micros -> Ptr Word8 -> IO ()
    injectTime t buf = pokeByteOff buf 8 $ encodeTimeToWord64 t
    {-# INLINE injectTime #-}

    injectNonce :: Nonce -> Ptr Word8 -> IO ()
    injectNonce n buf = poke (castPtr buf) $ encodeNonceToWord64 n
    {-# INLINE injectNonce #-}

    -- | `PowHashNat` interprets POW hashes as unsigned 256 bit integral numbers
    -- in little endian encoding.
    --
    fastCheckTarget :: Ptr Word64 -> Ptr Word64 -> IO Bool
    fastCheckTarget !trgPtr !powPtr =
        fastCheckTargetN 3 trgPtr powPtr >>= \case
            LT -> return False
            GT -> return True
            EQ -> fastCheckTargetN 2 trgPtr powPtr >>= \case
                LT -> return False
                GT -> return True
                EQ -> fastCheckTargetN 1 trgPtr powPtr >>= \case
                    LT -> return False
                    GT -> return True
                    EQ -> fastCheckTargetN 0 trgPtr powPtr >>= \case
                        LT -> return False
                        GT -> return True
                        EQ -> return True
    {-# INLINE fastCheckTarget #-}

    fastCheckTargetN :: Int -> Ptr Word64 -> Ptr Word64 -> IO Ordering
    fastCheckTargetN n trgPtr powPtr = compare
        <$> peekElemOff trgPtr n
        <*> peekElemOff powPtr n
    {-# INLINE fastCheckTargetN #-}
