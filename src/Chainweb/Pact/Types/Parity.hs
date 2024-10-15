{-# language
    DerivingStrategies
    , GeneralizedNewtypeDeriving
    , ImportQualifiedPost
    , LambdaCase
    , OverloadedStrings
    , RecordWildCards
    , ViewPatterns
#-}

module Chainweb.Pact.Types.Parity
    ( CommandResultDiffable(..)
    , commandResultToDiffable
    , replaceHash
    )
    where

import Chainweb.Miner.Pact (MinerId(..))
import Data.List qualified as List
import Data.Functor (void)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Maybe (fromMaybe)
import Pact.Core.Capabilities qualified as Pact5
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Errors qualified as Pact5
import Pact.Core.PactValue qualified as Pact5
import Pact.Core.Persistence qualified as Pact5
import Pact.Core.StableEncoding (StableEncoding(..))
import Pact.JSON.Encode qualified as J
import Text.Regex qualified as R

data CommandResultDiffable = CommandResultDiffable
    { -- _crdTxId :: Maybe Pact5.TxId -- TODO: Can't do txId for now
    -- TODO: include txlogs, after converting them to the same format
      _crdRequestKey :: Pact5.RequestKey
    , _crdResult :: Pact5.PactResult ErrorDiffable
    , _crdEvents :: OrderedEvents
    }
    deriving stock (Eq, Show)

instance J.Encode CommandResultDiffable where
    build CommandResultDiffable{..} = J.object
        [ --"txId" J..?= fmap (J.Aeson . Pact5._txId) _crdTxId
          "requestKey" J..= _crdRequestKey
        , "result" J..= _crdResult
        , "events" J..= _crdEvents
        ]

newtype OrderedEvents
    = OrderedEvents { getOrderedEvents :: Set (StableEncoding (Pact5.PactEvent Pact5.PactValue)) }
    deriving stock (Show)
    deriving newtype (Eq)

instance J.Encode OrderedEvents where
    build (OrderedEvents s) = J.array s

newtype ErrorDiffable
    = ErrorDiffable (Pact5.PactErrorCompat ())
    deriving stock (Show)

instance J.Encode ErrorDiffable where
    build (ErrorDiffable e) = J.build (fmap J.Array e)

instance Eq ErrorDiffable where
    ErrorDiffable ler == ErrorDiffable rer = diffErr ler rer
        where
        diffErr (Pact5.PELegacyError l) (Pact5.PELegacyError r) =
            Pact5._leType l == Pact5._leType r
        diffErr (Pact5.PEPact5Error erc) r =
            diffErr (Pact5.PELegacyError $ mkLegacyErrorFromCode erc) r
        diffErr l (Pact5.PEPact5Error erc) =
            diffErr l (Pact5.PELegacyError $ mkLegacyErrorFromCode erc)
        -- We destroy a bit of information whenever we make error codes, so
        -- what we can do, is recover what we can, which is the most important bit:
        -- the failure cause. We don't really care about the error message, callstack or info
        mkLegacyErrorFromCode (Pact5.prettyErrorCode -> e) =
            Pact5.LegacyPactError
            { Pact5._leType = getLegacyErrType e
            , Pact5._leInfo = mempty
            , Pact5._leCallStack = []
            , Pact5._leMessage = ""
            }
        getLegacyErrType e = case Pact5._pecFailurePhase e of
            "PEExecutionError" -> case Pact5._pecFailureCause e of
                "NativeArgumentsError" -> Pact5.LegacyArgsError
                "GasExceeded" -> Pact5.LegacyGasError
                "DbOpFailure" -> Pact5.LegacyDbError
                "ContinuationError" -> Pact5.LegacyContinuationError
                _ -> Pact5.LegacyEvalError
            "PEUserRecoverableError" -> Pact5.LegacyTxFailure
            "PEParseError" -> Pact5.LegacySyntaxError
            "PELexerError" -> Pact5.LegacySyntaxError
            "PEDesugarError" -> Pact5.LegacySyntaxError
            "PEVerifierError" -> Pact5.LegacyEvalError
            _ -> error "impossible: Pact 5 error code generated an illegal error code. This should never happen"

commandResultToDiffable :: ()
    => MinerId -- ^ filter out miner
    -> Pact5.CommandResult log (Pact5.PactErrorCompat info)
    -> CommandResultDiffable
commandResultToDiffable (MinerId minerId) cr = CommandResultDiffable
    { -- _crdTxId = Pact5._crTxId cr
      _crdRequestKey = Pact5._crReqKey cr
    , _crdResult = filterModuleHash $ Pact5._crResult (ErrorDiffable . void <$> cr)
    , _crdEvents = OrderedEvents $ Set.fromList $ fmap StableEncoding (List.filter (not . isMinerEvent) (Pact5._crEvents cr))
    }
    where
        isMinerEvent pe = Pact5.PString minerId `List.elem` Pact5._peArgs pe

        filterModuleHash :: Pact5.PactResult a -> Pact5.PactResult a
        filterModuleHash = \case
            Pact5.PactResultErr err -> Pact5.PactResultErr err
            Pact5.PactResultOk pv -> Pact5.PactResultOk $ case pv of
                Pact5.PString s -> Pact5.PString $ replaceHash s
                _ -> pv

-- Function to replace the hash in the "Loaded module ..., hash ..." string
replaceHash :: Text -> Text
replaceHash (Text.unpack -> input) = Text.pack $ R.subRegex regex input replacement
    where
        -- Regex to match 'Loaded module <any ascii string without spaces>, hash <hash>'
        regex = R.mkRegex "^Loaded module [a-zA-Z0-9._-]+, hash [^ ]+"
        replacement = "hash <modulehash_placeholder>"
