{-# LANGUAGE BangPatterns #-}
module Chainweb.Pact5.Types
    ( TxContext(..)
    , guardCtx
    , ctxCurrentBlockHeight
    )
    where
import Chainweb.BlockHeader
import Pact.Core.ChainData
import Chainweb.Miner.Pact (Miner)
import Chainweb.Time
import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.Version
import Chainweb.BlockHash
import Chainweb.Pact.Types (PactBlockM, psParentHeader)
import qualified Chainweb.ChainId
import Chainweb.Utils
import Control.Lens

-- | Pair parent header with transaction metadata.
-- In cases where there is no transaction/Command, 'PublicMeta'
-- default value is used.
data TxContext = TxContext
  { _tcParentHeader :: !ParentHeader
  , _tcMiner :: !Miner
  } deriving Show

instance HasChainId TxContext where
  _chainId = Chainweb.ChainId._chainId . _tcParentHeader
instance HasChainwebVersion TxContext where
  _chainwebVersion = _chainwebVersion . _tcParentHeader

-- | Convert context to datatype for Pact environment using the
-- current blockheight, referencing the parent header (not grandparent!)
-- hash and blocktime data
--
ctxToPublicData' :: PublicMeta -> TxContext -> PublicData
ctxToPublicData' pm ctx = PublicData
    { _pdPublicMeta = pm
    , _pdBlockHeight = bh
    , _pdBlockTime = bt
    , _pdPrevBlockHash = toText h
    }
  where
    bheader = _parentHeader (_tcParentHeader ctx)
    BlockHeight !bh = succ $ _blockHeight bheader
    BlockCreationTime (Time (TimeSpan (Micros !bt))) =
      _blockCreationTime bheader
    BlockHash h = _blockHash bheader

-- | Retrieve parent header as 'BlockHeader'
ctxBlockHeader :: TxContext -> BlockHeader
ctxBlockHeader = _parentHeader . _tcParentHeader

-- | Get "current" block height, which means parent height + 1.
-- This reflects Pact environment focus on current block height,
-- which influenced legacy switch checks as well.
ctxCurrentBlockHeight :: TxContext -> BlockHeight
ctxCurrentBlockHeight = succ . _blockHeight . ctxBlockHeader

ctxChainId :: TxContext -> Chainweb.ChainId.ChainId
ctxChainId = _blockChainId . ctxBlockHeader

ctxVersion :: TxContext -> ChainwebVersion
ctxVersion = _chainwebVersion . ctxBlockHeader

guardCtx :: (ChainwebVersion -> Chainweb.ChainId.ChainId -> BlockHeight -> a) -> TxContext -> a
guardCtx g txCtx = g (ctxVersion txCtx) (ctxChainId txCtx) (ctxCurrentBlockHeight txCtx)

-- | Assemble tx context from transaction metadata and parent header.
getTxContext :: Miner -> PactBlockM logger db tbl TxContext
getTxContext miner = view psParentHeader >>= \ph -> return (TxContext ph miner)
