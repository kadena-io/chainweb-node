{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.SPV.XChan
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A proof-of-burn based approach for transferring fungible tokens between
-- blockchains.
--
module Chainweb.SPV.XChan
( XChanVersion(..)
, encodeXChanVersion
, decodeXChanVersion
, XChanPolicy(..)
, encodeXChanPolicy
, decodeXChanPolicy

-- * XChan
, XChan(..)
, XChanId(..)
, encodeXChanId
, decodeXChanId
, xChanRoot
, XChanProof(..)
, xChanProof
, xChanProof'
, runXChanProof

-- * XChanClaim
, XChanClaim(..)
, xChanClaim
, encodeXChanClaim
, decodeXChanClaim

-- * internal
, MerkleRoot(..)
, merkleRoot
, merkleHash
, MerkleProof(..)
, runProof

-- * Debugging
, Tree(..)
, merkleTree
, root
, xChanMerkleTree
-- , testXChan
-- , test
) where

import Control.Monad.Catch
import Chainweb.ChainId
import Chainweb.Core.Brief
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Control.Monad
import Data.Bits
import Data.ByteString.Short qualified as BS
import Data.Coerce
import Data.Hash.Blake2
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.String
import Data.Text qualified as T
import Data.Word
import Ethereum.Misc (BytesN, unsafeBytesN, _getBytesN)
import Ethereum.Utils qualified as E
import GHC.Num.Natural
import Text.Printf (printf)
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse

-- -------------------------------------------------------------------------- --
-- Utils

runPutBS :: Put -> BS.ShortByteString
runPutBS = BS.toShort . runPutS

-- -------------------------------------------------------------------------- --
-- Merkle Tree

-- Blake2s is ZK friendly. Or should we use one that is more Ethereum
-- friendly and maybe has an opcode already?
--
merkleHash :: BS.ShortByteString -> MerkleRoot
merkleHash = MerkleRoot . unsafeBytesN . coerce . hashShortByteString_ @Blake2s256

newtype MerkleRoot = MerkleRoot (BytesN 32)
    deriving (Eq, Ord, Show)

encodeMerklRoot :: MerkleRoot -> Put
encodeMerklRoot (MerkleRoot b) = putShortByteString $ _getBytesN b
{-# INLINE encodeMerklRoot #-}

decodeMerkleRoot :: Get MerkleRoot
decodeMerkleRoot = label "MerkleRoot" $ do
    b <- getShortByteString 32
    return $ MerkleRoot $ unsafeBytesN b
{-# INLINE decodeMerkleRoot #-}

merkleNode :: Enum tag => tag -> BS.ShortByteString -> MerkleRoot
merkleNode t b = merkleHash $ tag <> b
  where
    tag = BS.toShort
        $ runPutS
        $ encodeWordLe @Word16
        $ int
        $ fromEnum t

emptyNode :: MerkleRoot
emptyNode = merkleNode TagInner mempty

innerNode :: MerkleRoot -> MerkleRoot -> MerkleRoot
innerNode (MerkleRoot a) (MerkleRoot b) =
    merkleNode TagInner (_getBytesN a <> _getBytesN b)

merkleRoot :: [MerkleRoot] -> MerkleRoot
merkleRoot = go []
  where
    go :: [(Int, MerkleRoot)] -> [MerkleRoot] -> MerkleRoot

    -- we are done!
    go [(_, n)] [] = n

    -- contract stack
    go ((h0, n0):(h1, n1):t) a
        | h0 == h1 = go ((h0 + 1, innerNode n1 n0):t) a

    -- push next input
    go s (n:t) = go ((0, n):s) t

    -- no input left but still work to do on the stack
    -- (this could be optimized by precomputing empty nodes per level)
    go s [] = go ((0, emptyNode):s) []

-- | Merkle Proof for simultaneously proving inlcusion of multiple leafs.
--
-- This encoding is suited for the Merkle tree of an XChan. Another encoding
-- would be to use just a list of leafs with the respective heights.
--
data MerkleProof = MerkleProof
    { _merkleProofRoots :: ![MerkleRoot]
    , _merkleProofLeafs :: !(NE.NonEmpty (Word, MerkleRoot))
    }
    deriving (Show, Eq, Ord)

data InvalidMerkleProofException
    = EmptyMerkleProofException
    | InvalidMerkleProofException !MerkleProof
    deriving (Show, Eq, Ord)

instance Exception InvalidMerkleProofException

-- | Run a Merkle proof
--
runProof
    :: forall m
    . MonadThrow m
    => MerkleProof
    -> m MerkleRoot
runProof p = foldLeafs (_merkleProofLeafs p) (_merkleProofRoots p) [] >>= \case
    ([], [(_, r)]) -> return r
    _ -> throwM $ InvalidMerkleProofException p
  where
    -- Proof the subtree for the next leaf index up the height at which it does
    -- not yet cover the next leaf index.
    --
    -- When we are done with a leaf we put it on the stack and continue with
    -- the next leaf.
    --
    foldLeafs
        :: NE.NonEmpty (Word, MerkleRoot)
            -- ^ current Leafs (index, root)
        -> [MerkleRoot]
            -- ^ current roots
        -> [(Word, MerkleRoot)]
            -- ^ current stack (height, root)
        -> m ([MerkleRoot], [(Word, MerkleRoot)])
    foldLeafs ((xi, x) NE.:| []) r s = go maxBound xi r ((0,x):s)
    foldLeafs ((xi, x) NE.:| (x'@(xi',_):xt)) r s = do
        (r', s') <- go h xi r ((0,x):s)
        foldLeafs (x' NE.:| xt) r' s'
      where
        h = int $ finiteBitSize xi - countLeadingZeros (xor xi xi') - 1

    -- Proof the subtree for a given leaf index up to the the given height.
    --
    -- Do this until we reach a height such that the next height would covers the
    -- next leaf index.
    --
    go
        :: MonadThrow m
        => Word
            -- ^ height where to stop (when we have to start with the next leaf)
        -> Word
            -- cur leaf trace
        -> [MerkleRoot]
            -- ^  current roots
        -> [(Word, MerkleRoot)]
            -- ^ current stack
        -> m ([MerkleRoot], [(Word, MerkleRoot)])
            -- ^ new stack and new roots
    go mh _ r s@((sh0, _) : _)
        | sh0 == mh = return (r, s)
    go _ 0 [] [x] = return ([], [x])

    go mh n r ((sh0, s0) : (sh1, s1) : st)
        | odd n && sh0 == sh1 =
            -- merge on stack to the left
            go mh (shiftR n 1) r ((sh0 + 1, innerNode s1 s0) : st)
    go mh n (r : rs) ((sh, s) : st)
        | odd n =
            -- merge with roots to the left
            go mh (shiftR n 1) rs ((sh + 1, innerNode r s) : st)
        | otherwise =
            -- merge with roots to the right
            go mh (shiftR n 1) rs ((sh + 1, innerNode s r) : st)

    -- This can not happen: we never call 'go' with an empty stack. And we only
    -- replace the top two elements of the stack with a new element.
    go _ _ _ [] = error "runProof: impossible empty stack. This is a bug."

    -- We are not yet done, because the trace is not 0. But we have no roots
    -- left, nothing to merge on the stack, and no additional leaves
    -- available to join with at the curent height. So we are stuck.
    go _ _ [] [_] = throwM $ InvalidMerkleProofException p

    -- We need to merge with a root, but there are no roots left.
    go _ _ [] _ = throwM $ InvalidMerkleProofException p

-- -------------------------------------------------------------------------- --
-- Explicit Merkle Tree for Debugging

data Tree
    = L MerkleRoot
    | N MerkleRoot Tree Tree
    deriving (Show, Eq, Ord)

root :: Tree -> MerkleRoot
root (L r) = r
root (N r _ _) = r

merkleTree :: [MerkleRoot] -> Tree
merkleTree = go []
  where
    go :: [(Int, Tree)] -> [MerkleRoot] -> Tree

    -- we are done!
    go [(_, t)] [] = t

    -- contract stack
    go ((h0, n0):(h1, n1):t) a
        | h0 == h1 = go ((h0 + 1, node n1 n0) : t) a

    -- push next input
    go s (n:t) = go ((0, L n):s) t

    -- no input left but still work to do on the stack
    -- (this could be optimized by precomputing empty nodes per level)
    go s [] = go ((0, L emptyNode):s) []

    node :: Tree -> Tree -> Tree
    node a b = N (innerNode (root a) (root b)) a b

-- -------------------------------------------------------------------------- --
-- XChan

data XChanVersion
    = XChainVersion0
    deriving (Show, Eq, Ord)

encodeXChanVersion :: XChanVersion -> Put
encodeXChanVersion XChainVersion0 = putWord16le 0

decodeXChanVersion :: Get XChanVersion
decodeXChanVersion = label "XChanVersion" $ getWord16le >>= \case
    0 -> return XChainVersion0
    n -> throwM $ DecodeException $ "XChanVersion: unknown version: " <> sshow n

-- | In the future more policies may be supported. Policies are dijunctive: funds
-- in a channel without policy can't be redeemed (policy is always "false"). If
-- there are more than one policy specified in a channel, the receiver can pick
-- which one is included in the proof. Exactly one policy must be provided in
-- order to receive funds from the channel. It is the responsibility of the
-- receiving contract to enforce the policy.
--
data XChanPolicy
    = TrgAccount BS.ShortByteString
        -- ^ In Version 1 the only supported policy is specifying the target
        -- account. If provided more than once the receiver can pick the target
        -- account. If omitted, nobody can redeem the channel.
        --
    deriving (Show, Eq, Ord)

encodeXChanPolicy :: XChanPolicy -> Put
encodeXChanPolicy (TrgAccount bs) = do
    putWord32le 0
    putWord32le $ int $ BS.length bs
    putShortByteString bs

decodeXChanPolicy :: Get XChanPolicy
decodeXChanPolicy = label "XChanPolicy" $ getWord32le >>= \case
    0 -> TrgAccount <$> do
        l <- getWord32le
        getShortByteString (int l)
    t -> throwM $ DecodeException $ "XChanPolicy: unknown tag: " <> sshow t

newtype XChanId = XChanId MerkleRoot
    deriving (Eq, Ord, Show)

encodeXChanId :: XChanId -> Put
encodeXChanId (XChanId r) = encodeMerklRoot r

decodeXChanId :: Get XChanId
decodeXChanId = label "XChanId" $ XChanId <$> decodeMerkleRoot

-- | A XChan is represents a channel for transfering funds of fungible tokens
-- between accounts on different block chains. It implements a proof-of-burn
-- approach, where the tokens as submitted to a channel by burning them on the
-- source chain. The tokens can be redeemed on the target chain by presenting
-- a proof of the burn. The proof may include additional metadata of the channel
-- that constraints how the funds can be redeemed on the target chain.
--
data XChan = XChan
    { _xVersion :: !XChanVersion
        -- ^ The version of the channel.
    , _xTrgChain :: !ChainId
        -- ^ The target chain on which the funds in the channel can be
        -- redeemed. This field must be verified by the receiver. In particular
        -- it must be verified that each channel has a *unique* target chain.
    , _xPolicy :: ![XChanPolicy]
        -- ^ A policy that constrains how funds can be redeemed on
        -- the target chain. In the simplest case this is the receiver account.
        -- Other possible examples include a time lock, an
        -- expiration time a multisig, a minimum or maximum redeem amount,
        -- some condition on the target chain, or a proof of off-chain
        -- condition.
    , _xData :: !BS.ShortByteString
        -- ^ arbitrary custom data that additionally identifies the channel.
        -- This field is ignored during verification.
        --
        -- This should include the source chain of the channel. This prevents
        -- loss of funds by sending tokens to the same channel on two different
        -- chains.
    }
    deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------- --
-- Merkle Tree for X-Channels

data MerkleTag
    = TagInner
    | TagVersion
    | TagTrgChain
    | TagData
    | TagPolicy
    deriving (Show, Eq, Ord, Enum)

versionNode :: XChanVersion -> MerkleRoot
versionNode = merkleNode TagVersion . runPutBS . encodeXChanVersion

trgChainNode :: ChainId -> MerkleRoot
trgChainNode = merkleNode TagTrgChain . runPutBS . encodeChainId

dataNode :: XChan -> MerkleRoot
dataNode = merkleNode TagData . _xData

policyNode :: XChanPolicy -> MerkleRoot
policyNode = merkleNode TagPolicy . runPutBS . encodeXChanPolicy

xChanRoot :: XChan -> XChanId
xChanRoot c = XChanId $ merkleRoot
    $ versionNode (_xVersion c)
    : trgChainNode (_xTrgChain c)
    : dataNode c
    : [ policyNode x | x <- _xPolicy c]

-- -------------------------------------------------------------------------- --
-- XChanProof

-- | Proof for of properties of an XChan. It is proven that the version and
-- target chain are in the first and second position of the Merkle tree. The
-- policy is any policy in the tree and only inclusion is proven.
--
data XChanProof = XChanProof
    { _xChanProofVersion :: !XChanVersion
        -- ^ The version of the channel. The first position in the Merkle tree.
    , _xChanProofTrgChain :: !ChainId
        -- ^ The target chain of the channel. The second position in the Merkle
        -- tree.
    , _xChanProofPolicyIndex :: !Natural
        -- ^ The zero based index of the policy in the Merkle tree.
    , _xChanProofPolicy :: !XChanPolicy
        -- ^ The policy for redeeming from the the channel.
    , _xChanProofRoots :: ![MerkleRoot]
        -- ^ The auxiliary roots of the proof
    }
    deriving (Show, Eq, Ord)

-- | The claim of an XChan Proof.
--
data XChanClaim = XChanClaim
    { _xChanClaimVersion :: !XChanVersion
        -- ^ The version of the channel. The first position in the Merkle tree.
    , _xChanClaimTrgChain :: !ChainId
        -- ^ The target chain of the channel. The second position in the Merkle
        -- tree.
    , _xChanClaimPolicy :: !XChanPolicy
        -- ^ The policy for redeeming from the the channel.
    }
    deriving (Show, Eq, Ord)

encodeXChanClaim :: XChanClaim -> Put
encodeXChanClaim c = do
    encodeXChanVersion (_xChanClaimVersion c)
    encodeChainId (_xChanClaimTrgChain c)
    encodeXChanPolicy (_xChanClaimPolicy c)

decodeXChanClaim :: Get XChanClaim
decodeXChanClaim = label "XChanClaim" $ XChanClaim
    <$> decodeXChanVersion
    <*> decodeChainId
    <*> decodeXChanPolicy

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag XChanClaim where
    type Tag XChanClaim = 'XChanClaimTag
    toMerkleNode = encodeMerkleInputNode encodeXChanClaim
    fromMerkleNode = decodeMerkleInputNode decodeXChanClaim

instance Brief XChanClaim where
    brief c = "XChanClaim {"
        <> " version: " <> brief (_xChanClaimVersion c)
        <> ", trgChain: " <> brief (_xChanClaimTrgChain c)
        <> ", policy: " <> brief (_xChanClaimPolicy c)
        <> "}"

-- -------------------------------------------------------------------------- --
-- Proof Verification

-- | Evidence for the information in the X-channel is provided subject to the
-- returned Merkle root belonging to an address.
--
-- In order to be usable for proof-of-burn based x-chain transfers, it is
-- mandatory that the moving funds out of the address is only possible by
-- proving that the address is the public key of a secret key that is known to
-- the signer of the transaction.
--
runXChanProof :: forall m . MonadThrow m => XChanProof -> m XChanId
runXChanProof p = XChanId <$> runProof MerkleProof
    { _merkleProofRoots = _xChanProofRoots p
    , _merkleProofLeafs = (0, versionNode (_xChanProofVersion p)) NE.:|
        [ (1, trgChainNode (_xChanProofTrgChain p))
        , (int (_xChanProofPolicyIndex p), policyNode (_xChanProofPolicy p))
        ]
    }

-- -------------------------------------------------------------------------- --
-- Proof Creation

xChanProof
    :: MonadThrow m
    => XChan
    -> XChanPolicy
    -> m XChanProof
xChanProof c policy = case L.findIndex (== policy) (_xPolicy c) of
    Nothing -> throwM $ userError "XChanProof: policy not found in channel"
    Just n -> xChanProof' c (int n)

xChanProof'
    :: MonadThrow m
    => XChan
    -> Natural
        -- ^ The zero based index of the policy in the channel.
    -> m XChanProof
xChanProof' c n
    | n >= int (length (_xPolicy c)) = throwM $
        userError "XChanProof: policy index out of bounds"
    | otherwise = return XChanProof
        { _xChanProofVersion = _xVersion c
        , _xChanProofTrgChain = _xTrgChain c
        , _xChanProofPolicyIndex = n + 3
            -- +2 for the version and target chain, +1 for '_xData'.
        , _xChanProofPolicy = _xPolicy c !! int n
            -- FIXME: report proper error if is out of bounds
        , _xChanProofRoots = proofRoots
            -- FIXME: use dlist or accumulator.
        }
  where
    (_root, proofRoots) = go [] leafs

    -- during contraction: when one of the arguments covers the target index
    -- add the other argument to the result rooots.
    --
    go
        :: [(Int, MerkleRoot, Bool)]
            -- ^ stack
        -> [(MerkleRoot, Bool)]
            -- ^ remaining leafs and whether they are included in the proof.
        -> (MerkleRoot, [MerkleRoot])

    -- we are done!
    go [(_, x, True)] [] = (x, [])
    go [(_, _, False)] [] = error "XChanProof: something went wrong. This is a bug."

    -- contract stack
    go ((h0, n0, True):(h1, n1, False):t) a
        | h0 == h1 = (n1 :) <$> (go ((h0 + 1, innerNode n1 n0, True):t) a)
    go ((h0, n0, False):(h1, n1, True):t) a
        | h0 == h1 = (n0 :) <$> (go ((h0 + 1, innerNode n1 n0, True):t) a)
    go ((h0, n0, x):(h1, n1, _):t) a
        | h0 == h1 = go ((h0 + 1, innerNode n1 n0, x):t) a

    -- push next input
    go s ((x,y):t) = go ((0, x, y):s) t

    -- no input left but still work to do on the stack
    -- (this could be optimized by precomputing empty nodes per level)
    go s [] = go ((0, emptyNode, False):s) []

    leafs :: [(MerkleRoot, Bool)]
    leafs
        = (versionNode (_xVersion c), True)
        : (trgChainNode (_xTrgChain c), True)
        : (dataNode c, False)
        :
            [ (policyNode x, y)
            | (i, x) <- zip [0::Int ..] (_xPolicy c)
            , let y = i == int n
            ]

xChanClaim :: XChanProof -> XChanClaim
xChanClaim p = XChanClaim
    { _xChanClaimVersion = _xChanProofVersion p
    , _xChanClaimTrgChain = _xChanProofTrgChain p
    , _xChanClaimPolicy = _xChanProofPolicy p
    }

-- | For debugging purposes only. This is the Merkle tree of the XChan
--
xChanMerkleTree :: XChan -> Tree
xChanMerkleTree c = merkleTree leafs
  where
    leafs :: [MerkleRoot]
    leafs
        = versionNode (_xVersion c)
        : trgChainNode (_xTrgChain c)
        : dataNode c
        : (policyNode <$> _xPolicy c)

-- -- -------------------------------------------------------------------------- --
-- -- Payload Provider Interfaces.
-- --
-- -- TODO: move to the respective payload provider modules.
--
-- -- | Computes the address of the Channel as the merkle root of channel data. The
-- -- hash function must guarantee that knowlege of the preimage proves that it is
-- -- infeasible to derive the respective secret key of address (under the random
-- -- oracle model). Therefore the address cannot belong to an EOA (externally
-- -- owned account). Additionally, the preimage must not be a valid preimage of
-- -- the address of an contract account. This ensures that the funds cannot be
-- -- moved out of the address.
-- --
-- chanEvmAddr :: XChan -> Address
-- chanEvmAddr chan = address $ xChanRoot chan
--
-- -- | Computes the Pact account of the Channel as the merkle root of channel
-- -- data. The hash function must guarantee that knowlege of the preimage proves
-- -- that it is infeasible to derive the respective secret key of the account
-- -- (under the random oracle model). Therefore the funds cannot be moved out of
-- -- the account.
-- --
-- chanPactAccount :: XChan -> Account
-- chanPactAccount chan = kaccount $ xChanRoot chan

-- -------------------------------------------------------------------------- --
-- Redeem XChan
--
-- To redeem funds from an XChan, the following evidence must be provided:
--
-- 1. The XChanClaim, which includes the version, target chain and policy.
-- 2. The blanance of the account that is identified by the XChan.
-- 3. The root of the blance proof is included in the targeted chain.

-- -------------------------------------------------------------------------- --
-- Brief Instances

instance Brief MerkleRoot where
    brief (MerkleRoot b) = briefJson $ E.HexBytes $ _getBytesN b

instance Brief XChanId where
    brief (XChanId b) = brief b

instance Brief Tree where
    brief (L r) = brief r
    brief (N r a b) = "[" <> brief r <> ": " <> brief a <> " | " <> brief b <> "]"

instance Brief XChanPolicy where
    brief (TrgAccount a) = "TrgAccount " <> briefJson (E.HexBytes a)

instance Brief XChanVersion where
    brief XChainVersion0 = "XChainVersion0"

instance Brief XChan where
    brief c = "XChan {"
        <> " version: " <> brief (_xVersion c)
        <> ", trgChain: " <> brief (_xTrgChain c)
        <> ", policy: " <> brief (_xPolicy c)
        <> ", data: " <> briefJson (E.HexBytes (_xData c))
        <> "}"

instance Brief XChanProof where
    brief p = "XChanProof {"
        <> " version: " <> brief (_xChanProofVersion p)
        <> ", trgChain: " <> brief (_xChanProofTrgChain p)
        <> ", policyIndex: " <> sshow (_xChanProofPolicyIndex p)
        <> ", policy: " <> brief (_xChanProofPolicy p)
        <> ", roots: " <> brief (_xChanProofRoots p)
        <> "}"

-- -------------------------------------------------------------------------- --
-- Debugging Code

test :: IO ()
test = forM_ [1..10] $ \i -> do
    let c = testXChan i
    forM_ [0..i - 1] $ go c
  where
    go c i = do
        proof <- xChanProof' c i
        r <- runXChanProof proof
        unless (r == xChanRoot c)
            $ error $ T.unpack
                $ "testXChan: invalid result of runXChanProof"
                <> ";\n expected: " <> brief (xChanRoot c)
                <> ";\n got: " <> brief r
                <> ";\n policy index: " <> sshow i
                <> ";\n channel: " <> brief c
                <> ";\n proof: " <> brief proof
                <> ";\n tree: " <> brief (xChanMerkleTree c)

testXChan :: Natural -> XChan
testXChan n = XChan
    { _xVersion = XChainVersion0
    , _xTrgChain = unsafeChainId 0
    , _xPolicy = policy
    , _xData = BS.toShort "test channel data"
    }
  where
    policy = [ TrgAccount (fromString $ printf "0x%040x" i) | i <- [0..n-1] ]
