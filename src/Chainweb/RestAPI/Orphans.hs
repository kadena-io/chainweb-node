{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.RestAPI.Orphan
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.RestAPI.Orphans () where

import Control.Lens
import Control.Monad

import Crypto.Hash.Algorithms

import Data.Aeson hiding (decode, encode)
import Data.Bifunctor
import Data.Bytes.Put
import Data.DoubleWord (Word128, Word256)
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import Data.Semigroup (Max(..), Min(..))
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as V4

import Numeric.Natural

import Servant.API

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Cut.CutHashes
import Chainweb.Difficulty hiding (properties)
import Chainweb.Graph
import Chainweb.HostAddress hiding (properties)
import Chainweb.MerkleLogHash (MerkleLogHash, merkleLogHashBytesCount)
import Chainweb.Miner.Core (ChainBytes, HeaderBytes, WorkBytes)
import Chainweb.Miner.Pact (Miner, defaultMiner)
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.SPV
import Chainweb.Time (Micros, Time, TimeSpan)
import Chainweb.TreeDB hiding (properties)
import Chainweb.Utils
import Chainweb.Utils.Paging hiding (properties)
import Chainweb.Version

import Pact.Parse (ParsedInteger(..))
import Pact.Server.API ()
import Pact.Types.Gas (GasLimit(..))

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- HttpApiData

instance ToHttpApiData GasLimit where
    toUrlPiece (GasLimit (ParsedInteger g)) = toUrlPiece g

instance FromHttpApiData GasLimit where
    parseUrlPiece p = GasLimit . ParsedInteger <$> parseUrlPiece p

instance ToHttpApiData PeerId where
    toUrlPiece = toText

instance FromHttpApiData PeerId where
    parseUrlPiece = first T.pack . eitherFromText

instance FromHttpApiData HostAddress where
    parseUrlPiece = first sshow . readHostAddressBytes . T.encodeUtf8

instance FromHttpApiData BlockHash where
    parseUrlPiece = first sshow
        . (runGet decodeBlockHash <=< decodeB64UrlNoPaddingText)

instance ToHttpApiData BlockHash where
    toUrlPiece = encodeB64UrlNoPaddingText . runPutS . encodeBlockHash

instance FromHttpApiData BlockPayloadHash where
    parseUrlPiece = first sshow
        . (runGet decodeBlockPayloadHash <=< decodeB64UrlNoPaddingText)

instance ToHttpApiData BlockPayloadHash where
    toUrlPiece = encodeB64UrlNoPaddingText . runPutS . encodeBlockPayloadHash

instance FromHttpApiData ChainwebVersion where
    parseUrlPiece = first T.pack . eitherFromText

instance ToHttpApiData ChainwebVersion where
    toUrlPiece = toText

instance FromHttpApiData ChainId where
    parseUrlPiece = first sshow . chainIdFromText

instance ToHttpApiData ChainId where
    toUrlPiece = chainIdToText

instance FromHttpApiData BlockHeight where
    parseUrlPiece = fmap BlockHeight . parseUrlPiece

instance ToHttpApiData BlockHeight where
    toUrlPiece (BlockHeight k) = toUrlPiece k

instance FromHttpApiData MinRank where
    parseUrlPiece = fmap (MinRank . Min) . parseUrlPiece

instance ToHttpApiData MinRank where
    toUrlPiece (MinRank (Min k)) = toUrlPiece k

instance FromHttpApiData MaxRank where
    parseUrlPiece = fmap (MaxRank . Max) . parseUrlPiece

instance ToHttpApiData MaxRank where
    toUrlPiece (MaxRank (Max k)) = toUrlPiece k

instance FromHttpApiData Eos where
    parseUrlPiece = fmap Eos . parseUrlPiece

instance ToHttpApiData Eos where
    toUrlPiece (Eos b) = toUrlPiece b

instance FromHttpApiData Limit where
    parseUrlPiece = fmap Limit . parseUrlPiece

instance ToHttpApiData Limit where
    toUrlPiece (Limit k) = toUrlPiece k

instance ToHttpApiData (NextItem BlockHash) where
    toUrlPiece = toText

instance FromHttpApiData (NextItem BlockHash) where
    parseUrlPiece = first sshow . nextItemFromText

instance ToHttpApiData (NextItem PeerId) where
    toUrlPiece = toText

instance FromHttpApiData (NextItem PeerId) where
    parseUrlPiece = first sshow . fromText

instance ToHttpApiData (NextItem Int) where
    toUrlPiece = toText

instance FromHttpApiData (NextItem Int) where
    parseUrlPiece = first sshow . fromText

instance ToHttpApiData SomeChainwebVersionT where
    toUrlPiece (SomeChainwebVersionT prox) = chainwebVersionSymbolVal prox

instance ToHttpApiData SomeChainIdT where
    toUrlPiece (SomeChainIdT prox) = chainIdSymbolVal prox

instance
    (KnownChainwebVersionSymbol sym, HasLink sub)
    => HasLink (sym :> sub)
  where
    type MkLink (sym :> sub) a = MkLink sub a
    toLink toA _ = toLink toA (Proxy @(ChainwebVersionSymbol sym :> sub))

instance
    (KnownChainIdSymbol sym, HasLink sub)
    => HasLink (sym :> sub)
  where
    type MkLink (sym :> sub) a = MkLink sub a
    toLink toA _ = toLink toA (Proxy @(ChainIdSymbol sym :> sub))

-- -------------------------------------------------------------------------- --
-- Swagger ParamSchema

instance ToParamSchema PeerId where
    toParamSchema _ = toParamSchema (Proxy @V4.UUID)

instance ToParamSchema BlockHash where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerString
        & format ?~ "byte"
        & maxLength ?~ int merkleLogHashBytesCount
        & minLength ?~ int merkleLogHashBytesCount

instance ToParamSchema BlockPayloadHash where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerString
        & format ?~ "byte"
        & maxLength ?~ int merkleLogHashBytesCount
        & minLength ?~ int merkleLogHashBytesCount

instance ToParamSchema BlockHeader where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerString
        & format ?~ "byte"

instance ToParamSchema BlockHeight where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerInteger
        & minimum_ ?~ 0
        & exclusiveMinimum ?~ False

instance ToParamSchema MinRank where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerInteger
        & minimum_ ?~ 0
        & exclusiveMinimum ?~ False

instance ToParamSchema MaxRank where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerInteger
        & minimum_ ?~ 0
        & exclusiveMinimum ?~ False

instance ToParamSchema Limit where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerInteger
        & minimum_ ?~ 0
        & exclusiveMinimum ?~ False

instance ToParamSchema Eos where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerBoolean

instance ToParamSchema (NextItem BlockHash) where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerString
        & pattern ?~ "(inclusive|exclusive):key"

instance ToParamSchema (NextItem PeerId) where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerString
        & pattern ?~ "(inclusive|exclusive):key"

instance ToParamSchema (NextItem Int) where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerString
        & pattern ?~ "(inclusive|exclusive):key"

instance ToParamSchema ChainId where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerInteger
        & format ?~ "word32"

-- FIXME: Invention of new `ChainwebVersion` values will not warn of pattern
-- match issues here!
instance ToParamSchema ChainwebVersion where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerString
        & enum_ ?~ (toJSON <$>
            [ Test petersonChainGraph
            , TimedConsensus petersonChainGraph
            , PowConsensus petersonChainGraph
            , TimedCPM petersonChainGraph
            , Development
            , Testnet02
            , Mainnet01
            ])

-- -------------------------------------------------------------------------- --
-- Swagger Schema

instance ToSchema Swagger where
    declareNamedSchema _ = return $ NamedSchema (Just "Swagger")
        $ sketchSchema ("swagger specification" :: T.Text)

instance ToSchema PeerInfo

instance ToSchema ChainId where
    declareNamedSchema _ = pure . NamedSchema (Just "ChainId") $ mempty
        & description ?~ "Unique identifier for a Chainweb Chain"
        & type_ .~ Just SwaggerInteger
        & example ?~ toJSON (someChainId Testnet02)

instance ToSchema PeerId where
    declareNamedSchema _ = declareNamedSchema (Proxy @V4.UUID)

instance ToSchema HostAddress where
    declareNamedSchema _ = return $ NamedSchema (Just "HostAddress") $ mempty
        & type_ .~ Just SwaggerString
        & pattern ?~ "<hostname>:<port>"
        & minLength ?~ 3
        & maxLength ?~ 258

instance ToSchema BlockHash where
    declareNamedSchema _ = return $ NamedSchema (Just "Key") $ byteSchema
        & minLength ?~ int merkleLogHashBytesCount
        & maxLength ?~ int merkleLogHashBytesCount

instance ToSchema BlockPayloadHash where
    declareNamedSchema _ = return $ NamedSchema (Just "BlockPayloadHash") $ byteSchema
        & minLength ?~ int merkleLogHashBytesCount
        & maxLength ?~ int merkleLogHashBytesCount

instance ToSchema BlockTransactionsHash where
    declareNamedSchema _ = return $ NamedSchema (Just "BlockTransactionsHash") $ byteSchema
        & minLength ?~ int merkleLogHashBytesCount
        & maxLength ?~ int merkleLogHashBytesCount

instance ToSchema BlockOutputsHash where
    declareNamedSchema _ = return $ NamedSchema (Just "BlockOutputsHash") $ byteSchema
        & minLength ?~ int merkleLogHashBytesCount
        & maxLength ?~ int merkleLogHashBytesCount

instance ToSchema Transaction where
    declareNamedSchema _ = return $ NamedSchema (Just "Transaction") $ byteSchema

instance ToSchema TransactionOutput where
    declareNamedSchema _ = return $ NamedSchema (Just "TransactionOutput") $ byteSchema

instance ToSchema MinerData where
    declareNamedSchema _ = return $ NamedSchema (Just "MinerData") $ byteSchema

instance ToSchema CoinbaseOutput where
    declareNamedSchema _ = return $ NamedSchema (Just "CoinbaseOutput") $ byteSchema

instance ToSchema (TransactionProof SHA512t_256) where
    declareNamedSchema _ = return $ NamedSchema (Just "TransactionProof") $ byteSchema

instance ToSchema (TransactionOutputProof SHA512t_256) where
    declareNamedSchema _ = return $ NamedSchema (Just "TransactionOutputProof") $ byteSchema

instance ToSchema TransactionOutputProofB64 where
    declareNamedSchema _ = return $ NamedSchema (Just "TransactionOutputProofB64") byteSchema

instance ToSchema PayloadData where
    declareNamedSchema _ = do
        transactionsSchema <- declareSchemaRef (Proxy @[Transaction])
        minerDataSchema <- declareSchemaRef (Proxy @MinerData)
        payloadHashSchema <- declareSchemaRef (Proxy @BlockPayloadHash)
        transactionsHashSchema <- declareSchemaRef (Proxy @BlockTransactionsHash)
        outputsHashSchema <- declareSchemaRef (Proxy @BlockOutputsHash)
        return $ NamedSchema (Just "PayloadData") $ mempty
            & type_ .~ Just SwaggerObject
            & properties .~
                [ ("transactions", transactionsSchema)
                , ("minerData", minerDataSchema)
                , ("payloadHash", payloadHashSchema)
                , ("transactionsHash", transactionsHashSchema)
                , ("outputsHash", outputsHashSchema)
                ]
            & required .~ [ "limit", "items" ]

instance ToSchema PayloadWithOutputs where
    declareNamedSchema _ = do
        transactionsWithOutputSchema <- declareSchemaRef (Proxy @[(Transaction, TransactionOutput)])
        minerDataSchema <- declareSchemaRef (Proxy @MinerData)
        coinbaseOutputSchema <- declareSchemaRef (Proxy @CoinbaseOutput)
        payloadHashSchema <- declareSchemaRef (Proxy @BlockPayloadHash)
        transactionsHashSchema <- declareSchemaRef (Proxy @BlockTransactionsHash)
        outputsHashSchema <- declareSchemaRef (Proxy @BlockOutputsHash)
        return $ NamedSchema (Just "PayloadWithOutputs") $ mempty
            & type_ .~ Just SwaggerObject
            & properties .~
                [ ("transactions", transactionsWithOutputSchema)
                , ("minerData", minerDataSchema)
                , ("coinbaseOutput", coinbaseOutputSchema)
                , ("payloadHash", payloadHashSchema)
                , ("transactionsHash", transactionsHashSchema)
                , ("outputsHash", outputsHashSchema)
                ]
            & required .~ [ "limit", "items" ]

instance ToSchema BlockHeader where
    declareNamedSchema _ = return $ NamedSchema (Just "BlockHeader") byteSchema

instance ToSchema CutHashes where
    declareNamedSchema _ = do
        mapSchema <- declareSchemaRef (Proxy @(HM.HashMap ChainId BlockHash))
        peerSchema <- declareSchemaRef (Proxy @PeerInfo)
        return $ NamedSchema (Just "CutHashes") $ mempty
            & type_ .~ Just SwaggerObject
            & properties .~
                [ ("hashes", mapSchema)
                , ("origin", peerSchema)
                ]
            & required .~ [ "hashes" ]

instance ToSchema (NextItem k) where
    declareNamedSchema _ = return $ NamedSchema (Just "next") $ mempty
        & type_ .~ Just SwaggerString
        & pattern ?~ "(inclusive|exclusive):<Key>"
        & minLength ?~ 10 + 1

instance (ToSchema a) => ToSchema (Page k a) where
    declareNamedSchema _ = do
        naturalSchema <- declareSchemaRef (Proxy @Natural)
        keySchema <- declareSchemaRef (Proxy @(NextItem k))
        itemsSchema <- declareSchemaRef (Proxy @[a])
        return $ NamedSchema (Just "Page") $ mempty
            & type_ .~ Just SwaggerObject
            & properties .~
                [ ("limit", naturalSchema)
                , ("items", itemsSchema)
                , ("next", keySchema)
                ]
            & required .~ [ "limit", "items" ]

instance ToSchema (BranchBounds BlockHeaderDb) where
    declareNamedSchema _ = do
        setSchema <- declareSchemaRef (Proxy @[BlockHash])
        return $ NamedSchema (Just "BranchBounds") $ mempty
            & type_ .~ Just SwaggerObject
            & properties .~
                [ ("upper", setSchema)
                , ("lower", setSchema)
                ]
            & required .~ [ "upper", "lower" ]

deriving instance ToSchema BlockCreationTime
deriving instance ToSchema BlockHashRecord
deriving instance ToSchema BlockHeight
deriving instance ToSchema BlockWeight
deriving instance ToSchema HashDifficulty
deriving instance ToSchema HashTarget
deriving instance ToSchema Micros
deriving instance ToSchema Nonce
deriving instance ToSchema PowHashNat
deriving instance ToSchema Word128
deriving instance ToSchema Word256
deriving instance ToSchema a => ToSchema (Time a)
deriving instance ToSchema a => ToSchema (TimeSpan a)
deriving instance ToSchema SpvRequest

instance ToSchema ChainwebVersion where
    declareNamedSchema _ = pure . NamedSchema (Just "ChainwebVersion") $ mempty
        & description ?~ "Unique identifier for a Chainweb network"
        & type_ .~ Just SwaggerString
        & example ?~ toJSON Testnet02

instance ToSchema MerkleLogHash where
    declareNamedSchema _ = pure $ NamedSchema (Just "MerkleLogHash") mempty

instance ToSchema HeaderBytes where
    declareNamedSchema _ = pure . NamedSchema (Just "HeaderBytes") $ binarySchema
        & description ?~ "An encoded BlockHeader"
        & minLength ?~ 302
        & maxLength ?~ 302

-- | See the docs for `WorkBytes` for justification of the byte length.
--
instance ToSchema WorkBytes where
    declareNamedSchema _ = pure . NamedSchema (Just "WorkBytes") $ binarySchema
        & description ?~ "The minimum information required to perform Proof-of-Work"
        & pattern ?~ "ChainBytes(4) + TargetBytes(32) + HeaderBytes(302)"
        & minLength ?~ (4 + 32 + 302)
        & maxLength ?~ (4 + 32 + 302)

instance ToSchema ChainBytes where
    declareNamedSchema _ = pure . NamedSchema (Just "ChainBytes") $ binarySchema
        & description ?~ "An encoded ChainId"
        & minLength ?~ 4
        & maxLength ?~ 4

instance ToSchema Miner where
    declareNamedSchema _ = do
        textSchema <- declareSchemaRef (Proxy @T.Text)
        listSchema <- declareSchemaRef (Proxy @[T.Text])
        pure . NamedSchema (Just "Miner") $ mempty
            & title ?~ "Miner Identity"
            & description ?~ "Information required to reward Miners for mining work"
            & type_ .~ Just SwaggerObject
            & properties .~
                [ ("account", textSchema)
                , ("public-keys", listSchema)
                , ("predicate", textSchema) ]
            & required .~ [ "account", "public-keys", "predicate" ]
            & example ?~ toJSON defaultMiner
