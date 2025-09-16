{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.SPV.VerifyProof
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Verification of Merkle proofs in the Chainweb Merkle tree.
--
module Chainweb.SPV.VerifyProof
(
    -- * Transaction Proofs
    runTransactionProof
    , verifyTransactionProof

    -- * Transaction Output Proofs
    , runTransactionOutputProof
    , verifyTransactionOutputProof
) where

import Chainweb.BlockHash
import Chainweb.BlockHeader (blockHeight)
import Chainweb.BlockHeight (BlockHeight)
import Chainweb.ChainId (ChainId)
import Chainweb.ChainId (_chainId)
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.Payload
import Chainweb.SPV
import Chainweb.BlockHeaderDB.HeaderOracle (HeaderOracle)
import Chainweb.BlockHeaderDB.HeaderOracle qualified as Oracle
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion, _chainwebVersion)
import Chainweb.Version.Guards qualified as CW
import Control.Lens (view)
import Control.Monad.Catch
import Crypto.Hash.Algorithms
import Data.MerkleLog
import Data.Text (Text)
import Prelude hiding (lookup)

-- -------------------------------------------------------------------------- --
-- Transaction Proofs

-- | Runs a transaction proof. Returns the block hash on the target chain for
--   which inclusion is proven.
--
runTransactionProof :: TransactionProof SHA512t_256 -> BlockHash
runTransactionProof (TransactionProof _ p) = BlockHash $ MerkleLogHash $ runMerkleProof p

-- | Verifies the proof against the current state of consensus. The result
-- confirms that the subject of the proof occurs in the history of the winning
-- fork of the target chain.
--
verifyTransactionProof
    :: HeaderOracle
    -> TransactionProof SHA512t_256
    -> IO Transaction
verifyTransactionProof oracle proof@(TransactionProof _cid p) = do
    let h = runTransactionProof proof
    whenM ((== Oracle.OutOfBounds) <$> Oracle.query oracle h) $ do
        let u = Oracle.upperBound oracle
        forkedError (_chainwebVersion u) (_chainId u) (view blockHeight u)
    proofSubject p

-- -------------------------------------------------------------------------- --
-- Output Proofs

-- | Runs a transaction output proof. Returns the block hash on the target chain for
--   which inclusion is proven.
--
runTransactionOutputProof :: TransactionOutputProof SHA512t_256 -> BlockHash
runTransactionOutputProof (TransactionOutputProof _ p) = BlockHash $ MerkleLogHash $ runMerkleProof p

-- | Verifies the proof for the given block hash. The result confirms that the
--   subject of the proof occurs in the history of the target chain before the
--   given block hash.
--
--   Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
--   the chain or when the given BlockHeaderDb is not for the target chain.
--
verifyTransactionOutputProof
    :: HeaderOracle
    -> TransactionOutputProof SHA512t_256
    -> IO TransactionOutput
verifyTransactionOutputProof oracle proof@(TransactionOutputProof _cid p) = do
    let h = runTransactionOutputProof proof
    whenM ((== Oracle.OutOfBounds) <$> Oracle.query oracle h) $ do
        let u = Oracle.upperBound oracle
        forkedError (_chainwebVersion u) (_chainId u) (view blockHeight u)
    proofSubject p

forkedError :: ChainwebVersion -> ChainId -> BlockHeight -> IO a
forkedError v cid h = throwM $ SpvExceptionVerificationFailed $ if CW.chainweb231Pact v cid h then errMsgPost231 else errMsgPre231
    where
        errMsgPre231 :: Text
        errMsgPre231 = "target header is not in the chain"

        errMsgPost231 :: Text
        errMsgPost231 = "target header is not in the chain or is out of bounds"
