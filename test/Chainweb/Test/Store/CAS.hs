{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Test.Store.CAS
  ( casDbTests
  , CasDbWithFunc(..)
  , MockPayload(..)
  , mockPayloadConfig
  ) where

------------------------------------------------------------------------------
import Control.DeepSeq
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Char8 (ByteString)
import Data.List
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Word (Word64)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (chooseAny)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))
------------------------------------------------------------------------------
import Chainweb.BlockHeader
import Chainweb.MerkleLogHash
import Chainweb.Store.CAS
import Chainweb.Utils (Codec(..))
------------------------------------------------------------------------------

data MockPayload = MockPayload !Word64 !Word64 !Word64 !Word64
  deriving (Show, Eq, Ord)

instance NFData MockPayload where
  rnf m = m `seq` ()

instance Arbitrary MockPayload where
    arbitrary = MockPayload <$> chooseAny
                            <*> chooseAny
                            <*> chooseAny
                            <*> chooseAny

hashMockPayload :: MockPayload -> BlockPayloadHash
hashMockPayload = BlockPayloadHash . unsafeMerkleLogHash . encodeMockPayload

encodeMockPayload :: MockPayload -> ByteString
encodeMockPayload (MockPayload a b c d) =
    runPutS (mapM_ putWord64le [a, b, c, d])

decodeMockPayload :: ByteString -> Either String MockPayload
decodeMockPayload s = flip runGetS s $ MockPayload
    <$> getWord64le
    <*> getWord64le
    <*> getWord64le
    <*> getWord64le

mockPayloadCodec :: Codec MockPayload
mockPayloadCodec = Codec encodeMockPayload decodeMockPayload

mockPayloadConfig :: PayloadConfig MockPayload
mockPayloadConfig = PayloadConfig mockPayloadCodec hashMockPayload

newtype CasDbWithFunc = CasDbWithFunc (forall a . (DB MockPayload -> IO a) -> IO a)

casDbTests :: CasDbWithFunc -> [TestTree]
casDbTests withDB = map ($ withDB) [
      payloadProperty "insert + lookup" (pick arbitrary) propInsert
    , payloadProperty "lookup failure" (pick arbitrary) propLookupFail
    , payloadProperty "delete" (pick arbitrary) propDelete
    ]

payloadProperty :: TestName
                -> PropertyM IO a
                -> (a -> DB MockPayload -> IO (Either String ()))
                -> CasDbWithFunc
                -> TestTree
payloadProperty name gen test (CasDbWithFunc withDB) = testProperty name go
  where
    go = monadicIO (gen >>= run . withDB . test >>= either fail return)


propInsert :: [MockPayload] -> DB MockPayload -> IO (Either String ())
propInsert payloads db = runExceptT $ do
    let payloadV = V.fromList payloads
    liftIO (casInsert db payloadV)
    lookups <- liftIO (casLookup db (V.map hash payloadV)) >>= mapM fromLookup
    when (lookups /= payloadV) $ fail "payload contents didn't match on lookup"

  where
    fromLookup = maybe (fail "expected lookup to succeed") return
    hash = payloadHash $ casDbConfig db


propLookupFail :: ([MockPayload], [MockPayload]) -> DB MockPayload -> IO (Either String ())
propLookupFail (ps0, fs0) db = runExceptT $ do
    liftIO (casInsert db (V.fromList ps))

    -- lookups on fs should fail (we didn't insert these)
    liftIO (casLookup db (V.fromList $ map hash fs)) >>= mapM_ checkLookupFailed

  where
    checkLookupFailed = maybe (return ()) (const $ fail "expected lookup to fail")
    ps = sort ps0
    psSet = Set.fromList ps
    fs = filter (not . flip Set.member psSet) fs0
    hash = payloadHash $ casDbConfig db


propDelete :: [MockPayload] -> DB MockPayload -> IO (Either String ())
propDelete payloads db = runExceptT $ do
    liftIO (casInsert db (V.fromList payloads))
    liftIO (casLookup db hashes) >>= mapM_ checkLookup
    liftIO (casDelete db hashes)
    liftIO (casLookup db hashes) >>= mapM_ checkLookupFailed

  where
    checkLookup = maybe (fail "expected lookup to succeed") (const $ return ())
    checkLookupFailed = maybe (return ()) (const $ fail "expected lookup to fail")
    deletes = V.fromList $ take 5 payloads
    hashes = V.map hash deletes
    hash = payloadHash $ casDbConfig db
