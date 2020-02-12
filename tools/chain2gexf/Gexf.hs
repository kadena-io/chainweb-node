{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Gexf
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Gexf
( blockHeaders2gexf
, blockMap2gexf
, readP2pSessions
, p2p2gexf
) where

import Control.Lens

import Data.Aeson
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T

import Text.XML.Generator

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockWeight
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.Time
import Chainweb.Utils

import P2P.Node
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Exported

blockHeaders2gexf :: Foldable f => f BlockHeader -> B.ByteString
blockHeaders2gexf = xrender
    . blocks2gexfDoc
    . HM.fromList
    . fmap (\i -> (i ^. blockHash, i))
    . toList

blockMap2gexf :: HM.HashMap BlockHash BlockHeader -> B.ByteString
blockMap2gexf = xrender . blocks2gexfDoc

blocks2gexfDoc :: HM.HashMap BlockHash BlockHeader -> Xml Doc
blocks2gexfDoc = gexf "chainweb-simulation" "chain" . blocks2graph

-- -------------------------------------------------------------------------- --
-- P2P Graph

readP2pSessions :: FilePath -> IO [P2pSessionInfo]
readP2pSessions file = fmap (either error id)
    . fmap eitherDecode
    . BL8.lines
    <$> BL8.readFile file

p2pInfo2edge :: P2pSessionInfo -> Gexf 'GexfEdge
p2pInfo2edge i = gexfEdge (_p2pSessionInfoId i)
    Nothing
    (Just 1)
    Nothing
    (pid $ _p2pSessionInfoSource i)
    (pid $ _p2pSessionInfoTarget i)
    start
    end
  where
    pid pinfo = toText (_hostAddressPort $ _peerAddr pinfo)
        <> "/" <> T.take 8 (maybe "?" toText $ _peerId pinfo)
    start = Just $ timeMs (_p2pSessionInfoStart i)
    end = timeMs <$> _p2pSessionInfoEnd i

    timeMs t = int . timeSpanMs $ t `diff` epoch

timeSpanMs :: Integral a => TimeSpan a -> a
timeSpanMs (TimeSpan x) = x `div` 1000

p2pEdges :: [P2pSessionInfo] -> Gexf 'GexfEdges
p2pEdges = gexfEdges . fmap p2pInfo2edge

p2p2graph :: [P2pSessionInfo] -> Gexf 'GexfDiGraph
p2p2graph es = gexfDiGraph
    (graphAttributes nodeAttr edgeAttr)
    (gexfNodes [])
    (p2pEdges es)
  where
    nodeAttr = []
    edgeAttr = []

p2p2gexf :: Foldable f => f P2pSessionInfo -> B.ByteString
p2p2gexf = xrender . p2p2gexfDoc . toList

p2p2gexfDoc :: [P2pSessionInfo] -> Xml Doc
p2p2gexfDoc = gexf "p2p-sessions" "chain" . p2p2graph

-- -------------------------------------------------------------------------- --
-- Encode Blocks as GEXF graph

type NodeId = T.Text
type EdgeId = T.Text
type Blocks = HM.HashMap BlockHash BlockHeader

blocks2graph :: Blocks -> Gexf 'GexfDiGraph
blocks2graph blocks = gexfDiGraph
    (graphAttributes nodeAttr edgeAttr)
    (nodes blocks)
    (edges blocks)
  where
    nodeAttr =
        [ ("chain", "int")
        , ("height", "int")
        , ("creationTime", "double")
        , ("miner", "string")
        , ("target", "integer")
        , ("weight", "integer")
        , ("nonce", "string")
        ]
    edgeAttr =
        [ ("isParent", "boolean")
        ]

nodes :: Blocks -> Gexf 'GexfNodes
nodes = gexfNodes . map block2node . HM.elems

edges :: Blocks -> Gexf 'GexfEdges
edges = gexfEdges
    . concatMap block2edges
    . HM.elems

block2node :: BlockHeader -> Gexf 'GexfNode
block2node b = gexfNode i i t $ Just
    [ ("chain", b ^. blockChainId . to toText)
    , ("height", sshow (b ^. blockHeight . to int :: Int))
    , ("creationTime", b ^. to creationTimeSeconds . to sshow)
    , ("target", sshow (log2 trg))
    , ("weight", sshow (log2 w))
    , ("nonce", sshow n)
    ]
  where
    i = toText $ b ^. blockHash
    t = Just $ creationTimeMs b
    HashTarget trg = _blockTarget b
    BlockWeight (HashDifficulty w) = _blockWeight b
    Nonce n = _blockNonce b
    log2 x = finiteBitSize x - 1 - countLeadingZeros x

block2edges :: BlockHeader -> [Gexf 'GexfEdge]
block2edges b
    | isGenesisBlockHeader b = []
    | otherwise = pe : ae
  where
    pe = gexfEdge (mkEdgeId i parentTarget) Nothing (Just 1)
        (Just [("isParent", "true")])
        i
        parentTarget
        t
        Nothing
    ae =
        [ gexfEdge eid Nothing Nothing (Just [("isParent", "false")]) i trg t Nothing
        | cid <- b ^. blockAdjacentHashes . getBlockHashRecord . to HM.elems
        , let trg = toText cid
        , let eid = mkEdgeId i trg
        ]
    t = Just $ creationTimeMs b
    i = toText $ b ^. blockHash
    parentTarget = toText $ b ^. blockParent

mkEdgeId :: NodeId -> NodeId -> EdgeId
mkEdgeId s t = s <> "$" <> t

creationTimeMs :: Integral a => BlockHeader -> a
creationTimeMs b = int . msecs $ t `diff` epoch
  where
    BlockCreationTime t = _blockCreationTime b
    msecs (TimeSpan x) = x `div` 1000

creationTimeSeconds :: BlockHeader -> Double
creationTimeSeconds b = secs $ t `diff` epoch
  where
    BlockCreationTime t = _blockCreationTime b
    secs (TimeSpan x) = int x / 1000000

-- -------------------------------------------------------------------------- --
-- GEXF Graph

data GexfElemType
    = GexfNode
    | GexfEdge
    | GexfNodes
    | GexfEdges
    | GexfDiGraph
    | GexfAttributes
    deriving (Show, Read, Eq)

newtype Gexf (a :: GexfElemType) = Gexf {xml :: Xml Elem }

gexf :: T.Text -> T.Text -> Gexf 'GexfDiGraph -> Xml Doc
gexf creator description graph = doc defaultDocInfo $
    xelemQ (namespace "" "http://www.gexf.net/1.2draft") "gexf" $ xelems
        [ xelem "meta" $ xelems
            [ xelem "creator" $ xtext creator
            , xelem "description" $ xtext description
            ]
        , xml graph
        ]

gexfDiGraph
    :: Gexf 'GexfAttributes
    -> Gexf 'GexfNodes
    -> Gexf 'GexfEdges
    -> Gexf 'GexfDiGraph
gexfDiGraph attributes ns es = Gexf $ xelem "graph"
    ( xattrs [ xattr "defaultedgetype" "directed", xattr "mode" "dynamic" ]
    , xelems
        [ xml attributes
        , xml ns
        , xml es
        ]
    )

graphAttributes :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> Gexf 'GexfAttributes
graphAttributes nodeAttributes edgeAttributes = Gexf $ xelems [nodeAttr, edgeAttr]
  where
    edgeAttr = xelem "attributes"
        ( xattr "class" "edge"
        , xelems
            [ xelem "attribute" $ xattrs [ xattr "id" (sshow i), xattr "title" title, xattr "type" typ ]
            | (title, typ) <- edgeAttributes
            | i <- [0 :: Int ..]
            ]
        )
    nodeAttr = xelem "attributes"
        ( xattr "class" "node"
        , xelems
            [ xelem "attribute" $ xattrs [ xattr "id" (sshow i), xattr "title" title, xattr "type" typ ]
            | (title, typ) <- nodeAttributes
            | i <- [0 :: Int ..]
            ]
        )

gexfNodes :: [Gexf 'GexfNode] -> Gexf 'GexfNodes
gexfNodes ns = Gexf $ xelem "nodes" $ xelems (xml <$> ns)

gexfEdges :: [Gexf 'GexfEdge] -> Gexf 'GexfEdges
gexfEdges es = Gexf $ xelem "edges" $ xelems (xml <$> es)

gexfNode
    :: NodeId
        -- ^ node id
    -> T.Text
        -- ^ node label
    -> Maybe Int
        -- ^ creation time
    -> Maybe [(T.Text, T.Text)]
        -- ^ node attribute values. The oder matters!
    -> Gexf 'GexfNode
gexfNode nid label t attributeValues = Gexf $ xelem "node" $
    ( xattrs $ catMaybes
        [ xattr "id" <$> pure nid
        , xattr "label" <$> pure label
        , xattr "start" . sshow <$> t
        ]
    , maybe noElems attr attributeValues
    )
  where
    attr values = xelem "attvalues" $ xelems
        [ xelem "attvalue" $ xattrs [ xattr "for" (sshow i), xattr "value" value ]
        | (_title, value) <- values
        | i <- [ 0 :: Int ..]
        ]

gexfEdge
    :: EdgeId
        -- ^ edge id
    -> Maybe T.Text
        -- ^ optional label
    -> Maybe Double
        -- ^ optional weight
    -> Maybe [(T.Text, T.Text)]
        -- ^ optional attribute values. The oder matters!
    -> NodeId
        -- ^ source
    -> NodeId
        -- ^ traget
    -> Maybe Int
        -- ^ start time
    -> Maybe Int
        -- ^ end time
    -> Gexf 'GexfEdge
gexfEdge nid label weight attrValues src trg s e = Gexf $ xelem "edge"
    ( xattrs $ catMaybes
        [ xattr "id" <$> pure nid
        , xattr "label" . sshow <$> label
        , xattr "weight" . sshow <$> weight
        , xattr "source" <$> pure src
        , xattr "target" <$> pure trg
        , xattr "start" . sshow <$> s
        , xattr "end" . sshow <$> e
        ]
    , maybe noElems attrs attrValues
    )
  where
    attrs values = xelem "attvalues" $ xelems
        [ xelem "attvalue" $ xattrs [ xattr "for" (sshow i), xattr "value" value ]
        | (_title, value) <- values
        | i <- [0 :: Int ..]
        ]
