{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}

module PlonkBn254.Verify
(verifyProof
, SP1VKeyHash(..)
, Proof(..)
, PlonkVK(..)
, PublicInputs(..)
, PlonkVerifyResult(..)) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Language.Haskell.TH.Syntax (Lift)

foreign import ccall "verify_proof_ffi"
  c_verify_proof
    :: Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> CString
    -> Ptr Word8 -> CSize
    -> IO CInt

data PlonkError
    = BeyondTheModulus
    | Bsb22CommitmentMismatch
    | ChallengeAlreadyComputed
    | ChallengeNotFound
    | DSTTooLarge
    | EllTooLarge
    | InverseNotFound
    | InvalidNumberOfDigests
    | InvalidWitness
    | PairingCheckFailed
    | PreviousChallengeNotComputed
    | TranscriptError
    | PlonkVkeyHashMismatch
    | GeneralError
    | StringConversionError
    | UnknownError Int
    deriving (Show, Eq)


data PlonkVerifyResult
    = VerificationSuccessful
    | VerificationError PlonkError
    deriving Show

resultToPlonkResult :: CInt -> PlonkVerifyResult
resultToPlonkResult r
    | r == 0 = VerificationSuccessful
    | otherwise = VerificationError $ case r of
        (-1)  -> BeyondTheModulus
        (-2)  -> Bsb22CommitmentMismatch
        (-3)  -> ChallengeAlreadyComputed
        (-4)  -> ChallengeNotFound
        (-5)  -> DSTTooLarge
        (-6)  -> EllTooLarge
        (-7)  -> InverseNotFound
        (-8)  -> InvalidNumberOfDigests
        (-9)  -> InvalidWitness
        (-10) -> PairingCheckFailed
        (-11) -> PreviousChallengeNotComputed
        (-12) -> TranscriptError
        (-13) -> PlonkVkeyHashMismatch
        (-99) -> GeneralError
        (-100) -> StringConversionError
        _     -> UnknownError $ fromIntegral r

newtype SP1VKeyHash = SP1VKeyHash
    { _vmKey :: BS.ByteString }
    deriving (Show, Eq, Ord)

newtype Proof = Proof
    { _proof :: BS.ByteString }
    deriving (Show, Eq, Ord)

newtype PlonkVK = PlonkVK
    { _programId :: BS.ByteString }
    deriving (Show, Eq, Ord)
    deriving (Lift)

newtype PublicInputs = PublicInputs
    { _publicParameter :: BS.ByteString }
    deriving (Show, Eq, Ord)

verifyProof :: Proof -> PublicInputs -> SP1VKeyHash -> PlonkVK -> IO PlonkVerifyResult
verifyProof (Proof proof) (PublicInputs inputs) (SP1VKeyHash sp1vkeyHash) (PlonkVK plonkVK) =
  BSU.unsafeUseAsCStringLen proof $ \(proofPtr, proofLen) ->
    BSU.unsafeUseAsCStringLen inputs $ \(inputsPtr, inputsLen) ->
        BS.useAsCString sp1vkeyHash $ \sp1vkeyHashPtr ->
            BS.useAsCStringLen plonkVK $ \(plonkVkPtr, plonkVkLen) -> do
                let proofLen' = fromIntegral proofLen
                    inputsLen' = fromIntegral inputsLen
                    plonkVkLen' = fromIntegral plonkVkLen

                result <- c_verify_proof
                    (castPtr proofPtr) proofLen'
                    (castPtr inputsPtr) inputsLen'
                    sp1vkeyHashPtr
                    (castPtr plonkVkPtr) plonkVkLen'

                return $ resultToPlonkResult result
