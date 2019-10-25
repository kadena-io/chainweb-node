{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Test external mining code (to check that it produces correct nonce / etc).

module TestMiner (main) where

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
import System.IO
import qualified System.Path as P
import qualified System.Random.MWC as MWC

import Chainweb.Miner.Core
    (MiningResult(..), callExternalMiner, fastCheckTarget)
import Chainweb.PowHash
import Chainweb.Version


pMiner :: Parser FilePath
pMiner = strOption
    (long "miner" <> metavar "PATH" <> help "Path to miner executable.")

pTargetZeroes :: Parser Int
pTargetZeroes = option auto
    (long "num-target-zeroes" <> value 20
     <> help "how many target zeroes (out of a possible 64) to require at the hex suffix")

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

checkMinerOutput
    :: ByteString      -- ^ generated nonce
    -> ByteString      -- ^ target, in hex format
    -> ByteString      -- ^ source header
    -> IO (Bool, ByteString)
checkMinerOutput nonceB targetBytes blockBytes0 = do
    B.unsafeUseAsCString hashBytes $ \hashPtr ->
      B.unsafeUseAsCString targetBytes $ \targetPtr -> do
        ok <- fastCheckTarget (castPtr targetPtr) (castPtr hashPtr)
        return (ok, hashBytes)
  where
    !blockBytes = nonceB <> B.drop 8 blockBytes0
    hashBytes = SB.fromShort $ powHashBytes $ powHash Testnet02 blockBytes

makeBlock :: IO ByteString
makeBlock = MWC.withSystemRandom $ \gen -> do
    blockW8 <- replicateM 320 $ MWC.uniform gen
    (return $! B8.pack blockW8) :: IO ByteString

genOneBlockAndTest
    :: Int                      -- ^ num target hash zeroes
    -> FilePath                 -- ^ miner path
    -> IO ()
genOneBlockAndTest targetZeroes minerPath0 = do
    minerPath <- P.makeAbsolute $ P.fromFilePath minerPath0
    blockBytes <- makeBlock
    t1 <- getTime Monotonic
    (MiningResult nonceBytes _ _ errBytes) <-
        either fail return =<< callExternalMiner minerPath [] True targetBytes blockBytes
    t2 <- getTime Monotonic
    (ok, outHashBytes) <- checkMinerOutput nonceBytes targetBytes blockBytes
    if ok then reportOK (t2 - t1) else failHash nonceBytes outHashBytes errBytes
  where
    reportOK t = do
        let d = fromInteger (toNanoSecs t) / (1000000000 :: Double)
        let msg = B.concat [
              "Successful result from miner in ",
              B.pack (show d),
              " seconds.\n"
              ]
        B.hPutStr stderr msg

    targetHex = makeTarget targetZeroes
    targetBytes = fst $ B16.decode targetHex

    failHash nonceBytes outHashBytes errBytes = do
        B.hPutStr stderr $ mconcat [
            B.pack msg
            , "\n\nProcess stderr: \n"
            , errBytes
            , "\n\n"
            ]
        fail msg
      where
        msg = formatErr nonceBytes outHashBytes
        formatErr nb ohb = concat [ "FAILURE: miner outputted nonce "
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
