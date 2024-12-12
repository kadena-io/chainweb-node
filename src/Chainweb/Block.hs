-- | A type for "blocks" including their header and payload and outputs.
-- This is only for REST APIs; we do not use the outputs otherwise.
module Chainweb.Block
    (Block(..))
    where

import Chainweb.BlockHeader
import Chainweb.Payload

data Block = Block
    { _blockHeader :: !BlockHeader
    , _blockPayloadWithOutputs :: !PayloadWithOutputs
    }
    deriving (Eq, Show)
