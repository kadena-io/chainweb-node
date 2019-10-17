{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Test external mining code (to check that it produces correct nonce / etc).

module TestMiner (main) where

import qualified Control.Concurrent.Async as Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B8
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as SB
import qualified Data.ByteString.Unsafe as B
import Foreign.Ptr
import Options.Applicative
import System.Clock
import System.Exit
import qualified System.IO.Streams as Streams
import System.Process
import qualified System.Random.MWC as MWC

import Chainweb.Miner.Core (fastCheckTarget)
import Chainweb.PowHash
import Chainweb.Version


pMiner :: Parser FilePath
pMiner = strOption
    (long "miner" <> metavar "PATH" <> help "Path to miner executable.")

pTargetZeroes :: Parser Int
pTargetZeroes = option auto
    (long "num-target-zeroes" <> value 20
     <> help "how many leading zeroes (out of a possible 64) to require at the hex prefix")

pNumTrials :: Parser Int
pNumTrials = option auto
    (long "num-trials" <> value 1 <> help "how many mining trials to run")

pOpts :: Parser (Int, Int, FilePath)
pOpts = (,,) <$> pTargetZeroes <*> pNumTrials <*> pMiner


-- make the hex string then hex-decode it
makeTarget :: Int -> ByteString
makeTarget numZeroes = B.append (B.replicate numFs 'f') (B.replicate numZeroes '0')
  where
    numFs = max 0 (64 - numZeroes)

callMiner :: ByteString -> FilePath -> ByteString -> IO (ByteString, ByteString)
callMiner target minerPath blockBytes = bracketOnError startup kill go
  where
    targetHashStr = B.unpack target
    environment = [("TARGET_HASH", targetHashStr)]
    kill (_, _, _, ph) = terminateProcess ph
    go (pstdin, pstdout, pstderr, ph) = do
        Streams.writeTo pstdin $ Just blockBytes
        Streams.writeTo pstdin Nothing
        Async.withAsync (readThread pstdout) $ \stdoutThread ->
          Async.withAsync (readThread pstderr) $ \stderrThread -> do
            code <- waitForProcess ph
            (outbytes, errbytes) <- (,) <$> Async.wait stdoutThread
                                        <*> Async.wait stderrThread
            when (code /= ExitSuccess) $ do
                Streams.writeTo Streams.stderr $ Just $ B.concat [
                    "Got error from miner. Stderr was: ",
                    errbytes
                    ]
                fail "miner-failure"
            -- reverse -- we want little-endian
            return (B.reverse $ fst $ B16.decode outbytes,
                    errbytes)

    startup = do
        Streams.writeTo Streams.stderr $ Just $ B.concat [
              "invoking '"
            , B.pack minerPath
            , " "
            , target
            , "'\n"
            ]
        Streams.runInteractiveProcess minerPath [targetHashStr] Nothing (Just environment)
    readThread s = B.concat <$> Streams.toList s


checkMinerOutput
    :: ByteString      -- ^ generated nonce
    -> ByteString      -- ^ target, in hex format
    -> ByteString      -- ^ source header
    -> IO (Bool, ByteString)
checkMinerOutput nonceB targetHex blockBytes0 = do
    B.unsafeUseAsCString hashBytes $ \hashPtr ->
      B.unsafeUseAsCString targetBytes $ \targetPtr -> do
        ok <- fastCheckTarget (castPtr targetPtr) (castPtr hashPtr)
        return (ok, hashBytes)
  where
    !blockBytes = nonceB <> B.drop 8 blockBytes0
    hashBytes = SB.fromShort $ powHashBytes $ powHash Testnet02 blockBytes
    targetBytes = fst $ B16.decode targetHex

makeBlock :: IO ByteString
makeBlock = MWC.withSystemRandom $ \gen -> do
    blockW8 <- replicateM 320 $ MWC.uniform gen
    (return $! B8.pack blockW8) :: IO ByteString

genOneBlockAndTest
    :: Int                      -- ^ num target hash zeroes
    -> FilePath                 -- ^ miner path
    -> IO ()
genOneBlockAndTest targetZeroes minerPath = do
    blockBytes <- makeBlock
    t1 <- getTime Monotonic
    (nonceBytes, errBytes) <- callMiner targetHex minerPath blockBytes
    t2 <- getTime Monotonic
    (ok, outHashBytes) <- checkMinerOutput nonceBytes targetHex blockBytes
    if ok then reportOK (t2 - t1) else failHash nonceBytes outHashBytes errBytes blockBytes
  where
    reportOK t = do
        let d = fromInteger (toNanoSecs t) / (1000000000 :: Double)
        let msg = B.concat [
              "Successful result from miner in ",
              B.pack (show d),
              "seconds.\n"
              ]
        Streams.writeTo Streams.stderr $ Just msg
    targetHex = makeTarget targetZeroes

    failHash nonceBytes outHashBytes errBytes blockBytes = do
        Streams.writeTo Streams.stderr $ Just $ mconcat [
            B.pack msg
            , "\n\nProcess stderr: \n"
            , errBytes
            , "\n\n"
            ]
        -- DEBUG
        (ok', outHashBytes') <- checkMinerOutput (B.reverse nonceBytes) targetHex blockBytes
        Streams.writeTo Streams.stderr $ Just $ mconcat [
            "\n\n\nDEBUG:\n\n"
            , "ok'="
            , B.pack (show ok')
            , "\n"
            , B.pack (format (B.reverse nonceBytes) outHashBytes')
            ]
        fail msg
      where
        msg = format nonceBytes outHashBytes
        format nb ohb = concat [ "FAILURE: miner outputted nonce "
                               , B.unpack $ B16.encode nb
                               , ", resulting in hash "
                               , B.unpack $ B16.encode ohb
                               , ".\nThis does not match target hex "
                               , B.unpack targetHex
                               , "\n"
                               ]

main :: IO ()
main = do
    (targetZeroes, numTrials, minerPath) <- execParser opts
    replicateM_ numTrials $ genOneBlockAndTest targetZeroes minerPath
  where
    opts = info (pOpts <**> helper)
             (fullDesc <> header "test-miner - run and test external miners.")
