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
