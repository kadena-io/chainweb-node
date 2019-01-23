{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Test.BlockPayloadDB
  ( blockPayloadDBTests
  , PayloadDBWithFunc(..)
  , MockPayload(..)
  , mockPayloadConfig
  ) where

------------------------------------------------------------------------------
import Control.DeepSeq
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word64)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (chooseAny)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))
------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockPayloadDB
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
hashMockPayload = BlockPayloadHash . BlockHashBytes . encodeMockPayload

encodeMockPayload :: MockPayload -> ByteString
encodeMockPayload (MockPayload a b c d) =
    runPutS (mapM_ putWord64le [a, b, c, d])

decodeMockPayload :: ByteString -> Maybe MockPayload
decodeMockPayload s = toMaybe . flip runGetS s $ do
    a <- getWord64le
    b <- getWord64le
    c <- getWord64le
    d <- getWord64le
    return $ MockPayload a b c d
  where
    toMaybe = either (const Nothing) Just

mockPayloadCodec :: Codec MockPayload
mockPayloadCodec = Codec encodeMockPayload decodeMockPayload

mockPayloadConfig :: PayloadConfig MockPayload
mockPayloadConfig = PayloadConfig mockPayloadCodec hashMockPayload

data PayloadDBWithFunc = PayloadDBWithFunc (forall a . (DB MockPayload -> IO a) -> IO a)

blockPayloadDBTests :: PayloadDBWithFunc -> [TestTree]
blockPayloadDBTests withDB = map ($ withDB) [
      payloadProperty "insert + lookup" (pick arbitrary) propInsert
    , payloadProperty "lookup failure" (pick arbitrary) propLookupFail
    , payloadProperty "delete" (pick arbitrary) propDelete
    ]

payloadProperty :: TestName
                -> PropertyM IO a
                -> (a -> DB MockPayload -> IO (Either String ()))
                -> PayloadDBWithFunc
                -> TestTree
payloadProperty name gen test (PayloadDBWithFunc withDB) = testProperty name go
  where
    go = monadicIO (gen >>= run . withDB . test >>= either fail return)


propInsert :: [MockPayload] -> DB MockPayload -> IO (Either String ())
propInsert = undefined

propLookupFail :: [MockPayload] -> DB MockPayload -> IO (Either String ())
propLookupFail = undefined

propDelete :: [MockPayload] -> DB MockPayload -> IO (Either String ())
propDelete = undefined
