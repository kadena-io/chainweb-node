{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Chainweb.Pact.TransactionExec
-- Copyright   :  Copyright © 2018 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mark Nichols <mark@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability   :  experimental
--
-- Pact command execution and coin-contract transaction logic for Chainweb
--
module Chainweb.Pact.TransactionExec
( -- * Transaction Execution
  applyCmd
, applyGenesisCmd
, applyLocal
, applyExec
, applyExec'
, applyContinuation
, applyContinuation'
, runPayload
, readInitModules

  -- * Gas Execution
, buyGas

  -- * Coinbase Execution
, applyCoinbase
, EnforceCoinbaseFailure(..)

  -- * Command Helpers
, publicMetaOf
, networkIdOf
, gasSupplyOf
, gasPriceOf

  -- * Utilities
, buildExecParsedCode
, mkMagicCapSlot

) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import Data.Decimal (Decimal, roundTo)
import Data.Default (def)
import Data.Foldable (for_, traverse_)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))

-- internal Pact modules

import Pact.Eval (eval, liftTerm, lookupModule)
import Pact.Gas (freeGasEnv)
import Pact.Interpreter
import Pact.Native.Capabilities (evalCap)
import Pact.Parse (ParsedDecimal(..), parseExprs)
import Pact.Runtime.Capabilities (popCapStack)
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Hash as Pact
import Pact.Types.Logger hiding (logError)
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.Server
import Pact.Types.SPV

-- internal Chainweb modules

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Templates
import Chainweb.Pact.Transactions.UpgradeTransactions
import Chainweb.Pact.Types hiding (logError)
import Chainweb.Transaction
import Chainweb.Utils (encodeToByteString, sshow, tryAllSynchronous)
import Chainweb.Version as V


-- -------------------------------------------------------------------------- --

-- | "Magic" capability 'COINBASE' used in the coin contract to
-- constrain coinbase calls.
--
magic_COINBASE :: CapSlot UserCapability
magic_COINBASE = mkMagicCapSlot "COINBASE"

-- | "Magic" capability 'GAS' used in the coin contract to
-- constrain gas buy/redeem calls.
--
magic_GAS :: CapSlot UserCapability
magic_GAS = mkMagicCapSlot "GAS"

-- | "Magic" capability 'GENESIS' used in the coin contract to
-- constrain genesis-only allocations
--
magic_GENESIS :: CapSlot UserCapability
magic_GENESIS = mkMagicCapSlot "GENESIS"


-- | The main entry point to executing transactions. From here,
-- 'applyCmd' assembles the command environment for a command and
-- orchestrates gas buys/redemption, and executing payloads.
--
applyCmd
    :: ChainwebVersion
    -> Logger
      -- ^ Pact logger
    -> PactDbEnv p
      -- ^ Pact db environment
    -> Miner
      -- ^ The miner chosen to mine the block
    -> GasModel
      -- ^ Gas model (pact Service config)
    -> TxContext
      -- ^ tx metadata and parent header
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command PayloadWithText
      -- ^ command with payload to execute
    -> ModuleCache
      -- ^ cached module state
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyCmd v logger pdbenv miner gasModel txCtx spv cmdIn mcache0 =
    second _txCache <$!>
      runTransactionM cenv txst applyBuyGas
  where
    txst = TransactionState mcache0 mempty 0 Nothing (_geGasModel freeGasEnv)

    executionConfigNoHistory = mkExecutionConfig
      $ FlagDisableHistoryInTransactionalMode
      : ( [ FlagOldReadOnlyBehavior | isPactBackCompatV16 ]
          ++ [ FlagPreserveModuleNameBug | not isModuleNameFix ]
          ++ [ FlagPreserveNsModuleInstallBug | not isModuleNameFix2 ]
        )

    cenv = TransactionEnv Transactional pdbenv logger (ctxToPublicData txCtx) spv nid gasPrice
      requestKey (fromIntegral gasLimit) executionConfigNoHistory

    cmd = payloadObj <$> cmdIn
    requestKey = cmdToRequestKey cmd
    gasPrice = gasPriceOf cmd
    gasLimit = gasLimitOf cmd
    initialGas = initialGasOf (_cmdPayload cmdIn)
    nid = networkIdOf cmd
    currHeight = ctxCurrentBlockHeight txCtx
    isModuleNameFix = enableModuleNameFix v currHeight
    isModuleNameFix2 = enableModuleNameFix2 v currHeight
    isPactBackCompatV16 = pactBackCompat_v16 v currHeight

    redeemAllGas r = do
      txGasUsed .= fromIntegral gasLimit
      applyRedeem r

    applyBuyGas =
      catchesPactError (buyGas isPactBackCompatV16 cmd miner) >>= \case
        Left e -> fatal $ "tx failure for requestKey when buying gas: " <> sshow e
        Right _ -> checkTooBigTx initialGas gasLimit applyPayload redeemAllGas

    applyPayload = do
      txGasModel .= gasModel
      txGasUsed .= initialGas

      cr <- catchesPactError $! runPayload cmd managedNamespacePolicy
      case cr of
        Left e -> do
          r <- jsonErrorResult e "tx failure for request key when running cmd"
          redeemAllGas r
        Right r -> applyRedeem r

    applyRedeem cr = do
      txGasModel .= (_geGasModel freeGasEnv)

      r <- catchesPactError $! redeemGas cmd
      case r of
        Left e ->
          -- redeem gas failure is fatal (block-failing) so miner doesn't lose coins
          fatal $ "tx failure for request key while redeeming gas: " <> sshow e
        Right _ -> do
          logs <- use txLogs
          return $! set crLogs (Just logs) cr

applyGenesisCmd
    :: Logger
      -- ^ Pact logger
    -> PactDbEnv p
      -- ^ Pact db environment
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyGenesisCmd logger dbEnv spv cmd =
    second _txCache <$!> runTransactionM tenv txst go
  where
    nid = networkIdOf cmd
    rk = cmdToRequestKey cmd
    tenv = TransactionEnv
        { _txMode = Transactional
        , _txDbEnv = dbEnv
        , _txLogger = logger
        , _txPublicData = def
        , _txSpvSupport = spv
        , _txNetworkId = nid
        , _txGasPrice = 0.0
        , _txRequestKey = rk
        , _txGasLimit = 0
        , _txExecutionConfig = def
        }
    txst = TransactionState
        { _txCache = mempty
        , _txLogs = mempty
        , _txGasUsed = 0
        , _txGasId = Nothing
        , _txGasModel = _geGasModel freeGasEnv
        }

    interp = initStateInterpreter $ initCapabilities [magic_GENESIS, magic_COINBASE]

    go = do
      cr <- catchesPactError $! runGenesis cmd permissiveNamespacePolicy interp
      case cr of
        Left e -> fatal $ "Genesis command failed: " <> sshow e
        Right r -> r <$ debug "successful genesis tx for request key"

applyCoinbase
    :: ChainwebVersion
    -> Logger
      -- ^ Pact logger
    -> PactDbEnv p
      -- ^ Pact db environment
    -> Miner
      -- ^ The miner chosen to mine the block
    -> ParsedDecimal
      -- ^ Miner reward
    -> TxContext
      -- ^ tx metadata and parent header
    -> EnforceCoinbaseFailure
      -- ^ enforce coinbase failure or not
    -> CoinbaseUsePrecompiled
      -- ^ always enable precompilation
    -> ModuleCache
    -> IO (T2 (CommandResult [TxLog Value]) (Maybe ModuleCache))
applyCoinbase v logger dbEnv (Miner mid mks) reward@(ParsedDecimal d) txCtx
  (EnforceCoinbaseFailure enfCBFailure) (CoinbaseUsePrecompiled enablePC) mc
  | fork1_3InEffect || enablePC = do
    let (cterm, cexec) = mkCoinbaseTerm mid mks reward
        interp = Interpreter $ \_ -> do put initState; fmap pure (eval cterm)
    go interp cexec
  | otherwise = do
    cexec <- mkCoinbaseCmd mid mks reward
    let interp = initStateInterpreter initState
    go interp cexec
  where
    fork1_3InEffect = vuln797Fix v cid bh
    throwCritical = fork1_3InEffect || enfCBFailure
    ec = mkExecutionConfig
      [ FlagDisableModuleInstall
      , FlagDisableHistoryInTransactionalMode ]
    tenv = TransactionEnv Transactional dbEnv logger (ctxToPublicData txCtx) noSPVSupport
           Nothing 0.0 rk 0 ec
    txst = TransactionState mc mempty 0 Nothing (_geGasModel freeGasEnv)
    initState = setModuleCache mc $ initCapabilities [magic_COINBASE]
    rk = RequestKey chash
    parent = _tcParentHeader txCtx

    bh = ctxCurrentBlockHeight txCtx
    cid = V._chainId parent
    chash = Pact.Hash $ encodeToByteString $ _blockHash $ _parentHeader parent
        -- NOTE: it holds that @ _pdPrevBlockHash pd == encode _blockHash@
        -- NOTE: chash includes the /quoted/ text of the parent header.

    go interp cexec = evalTransactionM tenv txst $! do
      cr <- catchesPactError $!
        applyExec' interp cexec mempty chash managedNamespacePolicy

      case cr of
        Left e
          | throwCritical -> throwM $ CoinbaseFailure $ sshow e
          | otherwise -> (`T2` Nothing) <$> jsonErrorResult e "coinbase tx failure"
        Right er -> do
          debug
            $! "successful coinbase of "
            <> (T.take 18 $ sshow d)
            <> " to "
            <> sshow mid

          upgradedModuleCache <- applyUpgrades v cid bh
          void $! applyTwentyChainUpgrade v cid bh
          logs <- use txLogs

          return $! T2
            (CommandResult rk (_erTxId er) (PactResult (Right (last $ _erOutput er)))
              (_erGas er) (Just $ logs) (_erExec er) Nothing)
            upgradedModuleCache


applyLocal
    :: Logger
      -- ^ Pact logger
    -> PactDbEnv p
      -- ^ Pact db environment
    -> GasModel
      -- ^ Gas model (pact Service config)
    -> TxContext
      -- ^ tx metadata and parent header
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command PayloadWithText
      -- ^ command with payload to execute
    -> ModuleCache
    -> ExecutionConfig
    -> IO (CommandResult [TxLog Value])
applyLocal logger dbEnv gasModel txCtx spv cmdIn mc execConfig =
    evalTransactionM tenv txst go
  where
    cmd = payloadObj <$> cmdIn
    rk = cmdToRequestKey cmd
    nid = networkIdOf cmd
    chash = toUntypedHash $ _cmdHash cmd
    signers = _pSigners $ _cmdPayload cmd
    gasPrice = gasPriceOf cmd
    gasLimit = gasLimitOf cmd
    tenv = TransactionEnv Local dbEnv logger (ctxToPublicData txCtx) spv nid gasPrice
           rk (fromIntegral gasLimit) execConfig
    txst = TransactionState mc mempty 0 Nothing gasModel
    gas0 = initialGasOf (_cmdPayload cmdIn)

    applyPayload m = do
      interp <- gasInterpreter gas0
      cr <- catchesPactError $! case m of
        Exec em ->
          applyExec interp em signers chash managedNamespacePolicy
        Continuation cm ->
          applyContinuation interp cm signers chash managedNamespacePolicy

      case cr of
        Left e -> jsonErrorResult e "applyLocal"
        Right r -> return $! r { _crMetaData = Just (toJSON $ ctxToPublicData' txCtx) }

    go = checkTooBigTx gas0 gasLimit (applyPayload $ _pPayload $ _cmdPayload cmd) return


readInitModules
    :: Logger
      -- ^ Pact logger
    -> PactDbEnv p
      -- ^ Pact db environment
    -> TxContext
      -- ^ tx metadata and parent header
    -> IO ModuleCache
readInitModules logger dbEnv txCtx =
    evalTransactionM tenv txst go
  where
    rk = RequestKey chash
    nid = Nothing
    chash = pactInitialHash
    tenv = TransactionEnv Local dbEnv logger (ctxToPublicData txCtx) noSPVSupport nid 0.0
           rk 0 def
    txst = TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv)
    interp = defaultInterpreter
    die msg = throwM $ PactInternalError $ "readInitModules: " <> msg
    mkCmd = buildExecParsedCode Nothing
    run msg cmd = do
      er <- catchesPactError $!
        applyExec' interp cmd [] chash permissiveNamespacePolicy
      case er of
        Left e -> die $ msg <> ": failed: " <> sshow e
        Right r -> case _erOutput r of
          [] -> die $ msg <> ": empty result"
          (o:_) -> return o

    go :: TransactionM p ModuleCache
    go = do

      -- see if fungible-v2 is there
      checkCmd <- liftIO $ mkCmd "(contains \"fungible-v2\" (list-modules))"
      checkFv2 <- run "check fungible-v2" checkCmd
      hasFv2 <- case checkFv2 of
        (PLiteral (LBool b)) -> return b
        t -> die $ "got non-bool result from module read: " <> T.pack (showPretty t)

      -- load modules by referencing members
      refModsCmd <- liftIO $ mkCmd $ T.intercalate " " $
        [ "coin.MINIMUM_PRECISION"
        , "ns.GUARD_SUCCESS"
        , "gas-payer-v1.GAS_PAYER"
        , "fungible-v1.account-details"] ++
        [ "fungible-v2.account-details" | hasFv2 ]
      void $ run "load modules" refModsCmd

      -- return loaded cache
      use txCache



applyUpgrades
  :: ChainwebVersion
  -> V.ChainId
  -> BlockHeight
  -> TransactionM p (Maybe ModuleCache)
applyUpgrades v cid height
     | coinV2Upgrade v cid height = go
     | otherwise = return Nothing
  where
    installCoinModuleAdmin = set (evalCapabilities . capModuleAdmin) $ S.singleton (ModuleName "coin" Nothing)
    go = applyTxs (upgradeTransactions v cid)

    applyTxs txsIO = do
      infoLog "Applying upgrade!"
      txs <- map (fmap payloadObj) <$> liftIO txsIO

      --
      -- Note (emily): the historical use of 'mapM_' here means that we are not
      -- threading the updated module cache from tx to tx in our upgrades.
      -- This means that the outputed module cache is the set of modules
      -- loaded in the /last/ tx, and that result will go into the hash.
      --
      -- Whether this can be fixed in the future should be explored, but we've
      -- already built fixes to address this problem that also affect the
      -- hashes of later blocks.
      --

      local (set txExecutionConfig def) $
        mapM_ applyTx txs
      mc <- use txCache
      return $ Just mc

    interp = initStateInterpreter $ installCoinModuleAdmin $
      initCapabilities [mkMagicCapSlot "REMEDIATE"]

    applyTx tx = do
      infoLog $ "Running upgrade tx " <> sshow (_cmdHash tx)

      tryAllSynchronous (runGenesis tx permissiveNamespacePolicy interp) >>= \case
        Right _ -> return ()
        Left e -> do
          logError $ "Upgrade transaction failed! " <> sshow e
          void $ throwM e


applyTwentyChainUpgrade
    :: ChainwebVersion
    -> V.ChainId
    -> BlockHeight
    -> TransactionM p ()
applyTwentyChainUpgrade v cid bh
    | to20ChainRebalance v cid bh = do
      txlist <- liftIO $ twentyChainUpgradeTransactions v cid

      infoLog $ "Applying 20-chain upgrades on chain " <> sshow cid

      let txs = fmap payloadObj <$> txlist

      --
      -- Note (emily): This function does not need to care about
      -- module caching, because it is already seeded with the correct cache
      -- state, and is not updating the module cache, unlike 'applyUpgrades'.
      --

      traverse_ applyTx txs
    | otherwise = return ()
  where
    applyTx tx = do
      infoLog $ "Running 20-chain upgrade tx " <> sshow (_cmdHash tx)

      let i = initStateInterpreter
            $ initCapabilities [mkMagicCapSlot "REMEDIATE"]

      r <- tryAllSynchronous (runGenesis tx permissiveNamespacePolicy i)
      case r of
        Left e -> do
          logError $ "Upgrade transaction failed: " <> sshow e
          void $! throwM e
        Right _ -> return ()



jsonErrorResult
    :: PactError
    -> Text
    -> TransactionM p (CommandResult [TxLog Value])
jsonErrorResult err msg = do
    logs <- use txLogs
    gas <- view txGasLimit -- error means all gas was charged
    rk <- view txRequestKey
    l <- view txLogger

    liftIO
      $! logLog l "INFO"
      $! T.unpack msg
      <> ": " <> show rk
      <> ": " <> show err

    return $! CommandResult rk Nothing (PactResult (Left err))
      gas (Just logs) Nothing Nothing

runPayload
    :: Command (Payload PublicMeta ParsedCode)
    -> NamespacePolicy
    -> TransactionM p (CommandResult [TxLog Value])
runPayload cmd nsp = do
    g0 <- use txGasUsed
    interp <- gasInterpreter g0

    case payload of
      Exec pm ->
        applyExec interp pm signers chash nsp
      Continuation ym ->
        applyContinuation interp ym signers chash nsp

  where
    signers = _pSigners $ _cmdPayload cmd
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

-- | Run genesis transaction payloads with custom interpreter
--
runGenesis
    :: Command (Payload PublicMeta ParsedCode)
    -> NamespacePolicy
    -> Interpreter p
    -> TransactionM p (CommandResult [TxLog Value])
runGenesis cmd nsp interp = case payload of
    Exec pm ->
      applyExec interp pm signers chash nsp
    Continuation ym ->
      applyContinuation interp ym signers chash nsp
  where
    signers = _pSigners $ _cmdPayload cmd
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

-- | Execute an 'ExecMsg' and Return the result with module cache
--
applyExec
    :: Interpreter p
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p (CommandResult [TxLog Value])
applyExec interp em senderSigs hsh nsp = do
    EvalResult{..} <- applyExec' interp em senderSigs hsh nsp
    debug $ "gas logs: " <> sshow _erLogGas
    logs <- use txLogs
    rk <- view txRequestKey
    -- applyExec enforces non-empty expression set so `last` ok
    -- forcing it here for lazy errors. TODO NFData the Pacts
    lastResult <- return $!! last _erOutput
    return $! CommandResult rk _erTxId (PactResult (Right lastResult))
      _erGas (Just logs) _erExec Nothing

-- | Variation on 'applyExec' that returns 'EvalResult' as opposed to
-- wrapping it up in a JSON result.
--
applyExec'
    :: Interpreter p
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p EvalResult
applyExec' interp (ExecMsg parsedCode execData) senderSigs hsh nsp
    | null (_pcExps parsedCode) = throwCmdEx "No expressions found"
    | otherwise = do

      eenv <- mkEvalEnv nsp (MsgData execData Nothing hsh senderSigs)
      er <- liftIO $! evalExec interp eenv parsedCode

      for_ (_erExec er) $ \pe -> debug
        $ "applyExec: new pact added: "
        <> sshow (_pePactId pe, _peStep pe, _peYield pe, _peExecuted pe)

      -- set log + cache updates + used gas
      setTxResultState er

      return er


-- | Execute a 'ContMsg' and return the command result and module cache
--
applyContinuation
    :: Interpreter p
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p (CommandResult [TxLog Value])
applyContinuation interp cm senderSigs hsh nsp = do
    EvalResult{..} <- applyContinuation' interp cm senderSigs hsh nsp
    debug $ "gas logs: " <> sshow _erLogGas
    logs <- use txLogs
    rk <- view txRequestKey
    -- last safe here because cont msg is guaranteed one exp
    return $! (CommandResult rk _erTxId (PactResult (Right (last _erOutput)))
      _erGas (Just logs) _erExec Nothing)

-- | Execute a 'ContMsg' and return just eval result, not wrapped in a
-- 'CommandResult' wrapper
--
applyContinuation'
    :: Interpreter p
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p EvalResult
applyContinuation' interp cm@(ContMsg pid s rb d _) senderSigs hsh nsp = do
    eenv <- mkEvalEnv nsp $ MsgData d pactStep hsh senderSigs
    er <- liftIO $! evalContinuation interp eenv cm

    setTxResultState er

    return er
  where
    pactStep = Just $ PactStep s rb pid Nothing

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
buyGas :: Bool -> Command (Payload PublicMeta ParsedCode) -> Miner -> TransactionM p ()
buyGas isPactBackCompatV16 cmd (Miner mid mks) = go
  where
    sender = view (cmdPayload . pMeta . pmSender) cmd
    initState mc = setModuleCache mc $ initCapabilities [magic_GAS]

    run input = do
      (findPayer isPactBackCompatV16 cmd) >>= \r -> case r of
        Nothing -> input
        Just withPayerCap -> withPayerCap input

    (Hash chash) = toUntypedHash (_cmdHash cmd)
    bgHash = Hash (chash <> "-buygas")

    go = do
      mcache <- use txCache
      supply <- gasSupplyOf <$> view txGasLimit <*> view txGasPrice

      let (buyGasTerm, buyGasCmd) = mkBuyGasTerm mid mks sender supply
          interp mc = Interpreter $ \_input ->
            put (initState mc) >> run (pure <$> eval buyGasTerm)

      result <- applyExec' (interp mcache) buyGasCmd
        (_pSigners $ _cmdPayload cmd) bgHash managedNamespacePolicy

      case _erExec result of
        Nothing -> fatal "buyGas: Internal error - empty continuation"
        Just pe -> void $! txGasId .= (Just $! GasId (_pePactId pe))

findPayer
  :: Bool
  -> Command (Payload PublicMeta ParsedCode)
  -> Eval e (Maybe (Eval e [Term Name] -> Eval e [Term Name]))
findPayer isPactBackCompatV16 cmd = runMaybeT $ do
    (!m,!qn,!as) <- MaybeT findPayerCap
    pMod <- MaybeT $ lookupModule qn m
    capRef <- MaybeT $ return $ lookupIfaceModRef qn pMod
    return $ runCap (getInfo qn) capRef as
  where
    setEnvMsgBody v e = set eeMsgBody v e

    findPayerCap :: Eval e (Maybe (ModuleName,QualifiedName,[PactValue]))
    findPayerCap = preview $ eeMsgSigs . folded . folded . to sigPayerCap . _Just

    sigPayerCap (SigCapability q@(QualifiedName m n _) as)
      | n == "GAS_PAYER" = Just (m,q,as)
    sigPayerCap _ = Nothing

    gasPayerIface = ModuleName "gas-payer-v1" Nothing

    lookupIfaceModRef (QualifiedName _ n _) (ModuleData (MDModule (Module {..})) refs)
      | gasPayerIface `elem` _mInterfaces = HM.lookup n refs
    lookupIfaceModRef _ _ = Nothing

    mkApp i r as = App (TVar r i) (map (liftTerm . fromPactValue) as) i

    runCap i capRef as input = do
      let msgBody = enrichedMsgBody cmd
          enrichMsgBody | isPactBackCompatV16 = id
                        | otherwise = setEnvMsgBody msgBody
      ar <- local enrichMsgBody $
        evalCap i CapCallStack False $ mkApp i capRef as

      case ar of
        NewlyAcquired -> do
          r <- input
          popCapStack (const (return ()))
          return r
        _ -> evalError' i "Internal error, GAS_PAYER already acquired"

enrichedMsgBody :: Command (Payload PublicMeta ParsedCode) -> Value
enrichedMsgBody cmd = case (_pPayload $ _cmdPayload cmd) of
  Exec (ExecMsg (ParsedCode _ exps) userData) ->
    object [ "tx-type" A..= ( "exec" :: Text)
           , "exec-code" A..= map renderCompactText exps
           , "exec-user-data" A..= pactFriendlyUserData userData ]
  Continuation (ContMsg pid step isRollback userData proof) ->
    object [ "tx-type" A..= ("cont" :: Text)
           , "cont-pact-id" A..= pid
           , "cont-step" A..= (LInteger $ toInteger step)
           , "cont-is-rollback" A..= LBool isRollback
           , "cont-user-data" A..= pactFriendlyUserData userData
           , "cont-has-proof" A..= (LBool $ isJust proof)
           ]
  where
    pactFriendlyUserData Null = object []
    pactFriendlyUserData v = v

-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
redeemGas :: Command (Payload PublicMeta ParsedCode) -> TransactionM p ()
redeemGas cmd = do
    mcache <- use txCache

    gid <- use txGasId >>= \case
      Nothing -> fatal $! "redeemGas: no gas id in scope for gas refunds"
      Just g -> return g

    fee <- gasSupplyOf <$> use txGasUsed <*> view txGasPrice

    void $! applyContinuation (initState mcache) (redeemGasCmd fee gid)
      (_pSigners $ _cmdPayload cmd) (toUntypedHash $ _cmdHash cmd)
      managedNamespacePolicy

  where
    initState mc = initStateInterpreter
      $ setModuleCache mc
      $ initCapabilities [magic_GAS]

    redeemGasCmd fee (GasId pid) =
      ContMsg pid 1 False (object [ "fee" A..= fee ]) Nothing


-- ---------------------------------------------------------------------------- --
-- Utilities

-- | Initialize a fresh eval state with magic capabilities.
-- This is the way we inject the correct guards into the environment
-- during Pact code execution
--
initCapabilities :: [CapSlot UserCapability] -> EvalState
initCapabilities cs = set (evalCapabilities . capStack) cs def
{-# INLINABLE initCapabilities #-}

initStateInterpreter :: EvalState -> Interpreter p
initStateInterpreter s = Interpreter $ (put s >>)


-- | Check whether the cost of running a tx is more than the allowed
-- gas limit and do some action depending on the outcome
--
checkTooBigTx
    :: Gas
    -> GasLimit
    -> TransactionM p (CommandResult [TxLog Value])
    -> (CommandResult [TxLog Value] -> TransactionM p (CommandResult [TxLog Value]))
    -> TransactionM p (CommandResult [TxLog Value])
checkTooBigTx initialGas gasLimit next onFail
  | initialGas >= (fromIntegral gasLimit) = do
      txGasUsed .= (fromIntegral gasLimit) -- all gas is consumed

      let !pe = PactError GasError def []
            $ "Tx too big (" <> pretty initialGas <> "), limit "
            <> pretty gasLimit

      r <- jsonErrorResult pe "Tx too big"
      onFail r
  | otherwise = next

gasInterpreter :: Gas -> TransactionM db (Interpreter p)
gasInterpreter g = do
    mc <- use txCache
    return $ initStateInterpreter
        $ set evalLogGas (Just [("GTxSize",g)]) -- enables gas logging
        $ set evalGas g
        $ setModuleCache mc def

-- | Initial gas charged for transaction size
--   ignoring the size of a continuation proof, if present
--
initialGasOf :: PayloadWithText -> Gas
initialGasOf cmd = gasFee
  where
    feePerByte :: Rational = 0.01

    contProofSize =
      case _pPayload (payloadObj cmd) of
        Continuation (ContMsg _ _ _ _ (Just (ContProof p))) -> B.length p
        _ -> 0
    txSize = SB.length (payloadBytes cmd) - contProofSize

    costPerByte = fromIntegral txSize * feePerByte
    sizePenalty = txSizeAccelerationFee costPerByte
    gasFee = ceiling (costPerByte + sizePenalty)
{-# INLINE initialGasOf #-}

txSizeAccelerationFee :: Rational -> Rational
txSizeAccelerationFee costPerByte = total
  where
    total = (costPerByte / bytePenalty) ^ power
    bytePenalty = 512
    power :: Integer = 7
{-# INLINE txSizeAccelerationFee #-}

-- | Set the module cache of a pact 'EvalState'
--
setModuleCache
  :: ModuleCache
  -> EvalState
  -> EvalState
setModuleCache = set (evalRefs . rsLoadedModules)
{-# INLINE setModuleCache #-}

-- | Set tx result state
--
setTxResultState :: EvalResult -> TransactionM db ()
setTxResultState er = do
    txLogs <>= (_erLogs er)
    txCache .= (_erLoadedModules er)
    txGasUsed .= (_erGas er)
{-# INLINE setTxResultState #-}

-- | Make an 'EvalEnv' given a tx env + state
--
mkEvalEnv
    :: NamespacePolicy
    -> MsgData
    -> TransactionM db (EvalEnv db)
mkEvalEnv nsp msg = do
    tenv <- ask
    genv <- GasEnv
      <$> view (txGasLimit . to fromIntegral)
      <*> view txGasPrice
      <*> use txGasModel

    return $ setupEvalEnv (_txDbEnv tenv) Nothing (_txMode tenv)
      msg initRefStore genv
      nsp (_txSpvSupport tenv) (_txPublicData tenv) (_txExecutionConfig tenv)

-- | Managed namespace policy CAF
--
managedNamespacePolicy :: NamespacePolicy
managedNamespacePolicy = SmartNamespacePolicy False
  (QualifiedName (ModuleName "ns" Nothing) "validate" def)
{-# NOINLINE managedNamespacePolicy #-}

-- | Builder for "magic" capabilities given a magic cap name
--
mkMagicCapSlot :: Text -> CapSlot UserCapability
mkMagicCapSlot c = CapSlot CapCallStack cap []
  where
    mn = ModuleName "coin" Nothing
    fqn = QualifiedName mn c def
    cap = SigCapability fqn []
{-# INLINE mkMagicCapSlot #-}

-- | Build the 'ExecMsg' for some pact code fed to the function. The 'value'
-- parameter is for any possible environmental data that needs to go into
-- the 'ExecMsg'.
--
buildExecParsedCode :: Maybe Value -> Text -> IO (ExecMsg ParsedCode)
buildExecParsedCode value code = maybe (go Null) go value
  where
    go v = case ParsedCode code <$> parseExprs code of
      Right !t -> pure $! ExecMsg t v
      -- if we can't construct coin contract calls, this should
      -- fail fast
      Left err -> internalError $ "buildExecParsedCode: parse failed: " <> T.pack err

-- | Retrieve public metadata from a command
--
publicMetaOf :: Command (Payload PublicMeta ParsedCode) -> PublicMeta
publicMetaOf = _pMeta . _cmdPayload
{-# INLINE publicMetaOf #-}

-- | Retrieve the optional Network identifier from a command
--
networkIdOf :: Command (Payload PublicMeta ParsedCode) -> Maybe NetworkId
networkIdOf = _pNetworkId . _cmdPayload
{-# INLINE networkIdOf #-}

-- | Calculate the gas fee (pact-generate gas cost * user-specified gas price),
-- rounding to the nearest stu.
--
gasSupplyOf :: Gas -> GasPrice -> GasSupply
gasSupplyOf gas (GasPrice (ParsedDecimal gp)) = GasSupply (ParsedDecimal gs)
  where
    gs = toCoinUnit ((fromIntegral gas) * gp)
{-# INLINE gasSupplyOf #-}

-- | Round to the nearest Stu
--
toCoinUnit :: Decimal -> Decimal
toCoinUnit = roundTo 12
{-# INLINE toCoinUnit #-}

-- | Log request keys at DEBUG when successful
--
debug :: Text -> TransactionM db ()
debug s = do
    l <- view txLogger
    rk <- view txRequestKey
    liftIO $! logLog l "DEBUG" $! T.unpack s <> ": " <> show rk


-- | Denotes fatal failure points in the tx exec process
--
fatal :: Text -> TransactionM db a
fatal e = do
    l <- view txLogger
    rk <- view txRequestKey

    liftIO
      $! logLog l "ERROR"
      $! "critical transaction failure: "
      <> sshow rk <> ": " <> T.unpack e

    throwM $ PactTransactionExecError (fromUntypedHash $ unRequestKey rk) e

logError :: Text -> TransactionM db ()
logError msg = view txLogger >>= \l -> liftIO $! logLog l "ERROR" (T.unpack msg)

infoLog :: Text -> TransactionM db ()
infoLog msg = view txLogger >>= \l -> liftIO $! logLog l "INFO" (T.unpack msg)
