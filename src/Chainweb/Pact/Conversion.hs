{-# LANGUAGE ScopedTypeVariables, LambdaCase, BangPatterns, TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Pact.Conversion
  ( fromLegacyQualifiedName
  , fromLegacyPactValue)
  where

-- import qualified Pact.Types.Persistence as Legacy
import qualified Pact.Types.Term as Legacy
-- import qualified Pact.Types.Hash as Legacy
import qualified Pact.Types.Exp as Legacy
-- import qualified Pact.Types.Continuation as Legacy
-- import qualified Pact.Types.ChainId as Legacy
-- import qualified Pact.Types.Namespace as Legacy
-- import qualified Pact.Types.RowData as Legacy
import qualified Pact.Types.PactValue as Legacy

-- import Control.Monad.Reader
-- import Control.Monad.State.Strict
-- import Control.Monad.Except
-- import Data.ByteString (ByteString)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Pact.Core.ModRefs
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.PactValue
-- import Pact.Core.Builtin
-- import Pact.Core.ChainData
-- import Pact.Core.Persistence
-- import Pact.Core.DefPacts.Types
-- import Pact.Core.Namespace
-- import Pact.Core.Hash

-- import qualified Pact.JSON.Decode as JD

fromLegacyQualifiedName
  :: Legacy.QualifiedName
  -> QualifiedName
fromLegacyQualifiedName (Legacy.QualifiedName mn n _) =
  QualifiedName n (fromLegacyModuleName mn)

fromLegacyLiteral
  :: Legacy.Literal
  -> Either Literal PactValue
fromLegacyLiteral = \case
  Legacy.LString s -> Left (LString s)
  Legacy.LInteger i -> Left (LInteger i)
  Legacy.LDecimal d -> Left (LDecimal d)
  Legacy.LBool b -> Left (LBool b)
  Legacy.LTime l -> Right $ PTime l

fromLegacyPactId
  :: Legacy.PactId
  -> DefPactId
fromLegacyPactId (Legacy.PactId pid) = DefPactId pid

fromLegacyPactValue :: Legacy.PactValue -> Either String PactValue
fromLegacyPactValue = \case
  Legacy.PLiteral l -> pure $ either PLiteral id $ fromLegacyLiteral l
  Legacy.PList p -> do
    l <- traverse fromLegacyPactValue p
    pure (PList l)
  Legacy.PObject (Legacy.ObjectMap om) -> do
    om' <- traverse fromLegacyPactValue om
    pure (PObject $ M.mapKeys (\(Legacy.FieldKey k) -> Field k) om')
  Legacy.PGuard g -> case g of
    Legacy.GPact (Legacy.PactGuard p n) -> let
      p' = fromLegacyPactId p
      in pure (PGuard (GDefPactGuard (DefPactGuard p' n)))
    Legacy.GKeySet (Legacy.KeySet k pred') ->  let
      ks = S.map (PublicKeyText . Legacy._pubKey)  k
      p' = \case
        (Legacy.Name (Legacy.BareName bn _def))
          | bn == "keys-all" -> pure KeysAll
          | bn == "keys-any" -> pure KeysAny
          | bn == "keys-2"   -> pure Keys2
        (Legacy.Name (Legacy.BareName bn _def)) -> pure (CustomPredicate (TBN $ BareName bn))
        (Legacy.QName qn) -> pure (CustomPredicate (TQN $ fromLegacyQualifiedName qn))
        o -> Left $ "fromLegacyPactValue: pred invariant: " <> show o
      in (PGuard . GKeyset . KeySet ks <$> p' pred')
    Legacy.GKeySetRef (Legacy.KeySetName ksn ns) -> let
      ns' = fromLegacyNamespaceName <$> ns
      in pure (PGuard . GKeySetRef $ KeySetName ksn ns')
    Legacy.GModule (Legacy.ModuleGuard mn n) -> let
      mn' = fromLegacyModuleName mn
      in pure (PGuard $ GModuleGuard (ModuleGuard mn' n))
    Legacy.GUser (Legacy.UserGuard n a) -> case n of
      Legacy.QName n' -> do
        let qn = fromLegacyQualifiedName n'
        args <- traverse fromLegacyPactValue a
        pure (PGuard $ GUserGuard (UserGuard qn args))
      _ -> Left "fromLegacyPactValue: invariant"
    Legacy.GCapability (Legacy.CapabilityGuard n a i) -> do
      let qn = fromLegacyQualifiedName n
      args <- traverse fromLegacyPactValue a
      pure (PGuard $ GCapabilityGuard (CapabilityGuard qn args (fromLegacyPactId <$> i)))
  Legacy.PModRef (Legacy.ModRef mn mmn _) -> let
    mn' = fromLegacyModuleName mn
    imp = S.fromList $ maybe [] (map fromLegacyModuleName) mmn
    in pure (PModRef $ ModRef mn' imp)


-- fromLegacyKeySetName
--   :: Legacy.KeySetName
--   -> KeySetName
-- fromLegacyKeySetName (Legacy.KeySetName ksn ns)
--   = KeySetName ksn (fromLegacyNamespaceName <$> ns)

fromLegacyNamespaceName :: Legacy.NamespaceName -> NamespaceName
fromLegacyNamespaceName (Legacy.NamespaceName ns) = NamespaceName ns

fromLegacyModuleName
  :: Legacy.ModuleName
  -> ModuleName
fromLegacyModuleName (Legacy.ModuleName n ns)
  = ModuleName n (fromLegacyNamespaceName <$> ns)

-- decodeKeySet :: ByteString -> Maybe KeySet
-- decodeKeySet bs = do
--   obj <- JD.decodeStrict' bs
--   either (const Nothing) Just (fromLegacyKeySet obj)

-- fromLegacyKeySet
--   :: Legacy.KeySet
--   -> Either String KeySet
-- fromLegacyKeySet (Legacy.KeySet ks p) = do
--   let ks' = S.map fromLegacyPublicKeyText ks
--   pred' <- case p of
--     Legacy.Name (Legacy.BareName "keys-all" _) -> pure KeysAll
--     Legacy.Name (Legacy.BareName "keys-2" _) -> pure Keys2
--     Legacy.Name (Legacy.BareName "keys-any" _) -> pure KeysAny
--     Legacy.QName qn -> pure (CustomPredicate (TQN $ fromLegacyQualifiedName qn))
--     other -> Left $ "fromLegacyKeySet: pred invariant" <> show other
--   pure (KeySet ks' pred')

-- fromLegacyPublicKeyText
--   :: Legacy.PublicKeyText
--   -> PublicKeyText
-- fromLegacyPublicKeyText (Legacy.PublicKeyText t) = PublicKeyText t


-- fromNestedPactExec :: Bool -> Legacy.NestedPactExec -> Legacy.PactExec
-- fromNestedPactExec rollback (Legacy.NestedPactExec stepCount yield exec step pid cont nested) =
--   Legacy.PactExec stepCount yield exec step pid cont rollback nested

-- fromLegacyContinuation
--   :: Legacy.PactContinuation
--   -> Either String (DefPactContinuation QualifiedName PactValue)
-- fromLegacyContinuation (Legacy.PactContinuation n args) = do
--   n' <- toQualifiedName n
--   args' <- traverse fromLegacyPactValue args
--   pure (DefPactContinuation n' args')
--   where
--   toQualifiedName = \case
--     Legacy.QName qn -> pure (fromLegacyQualifiedName qn)
--     _ -> Left "fromLegacyContinuation invariant: expected qualified name"


-- fromLegacyYield :: Legacy.Yield -> Either String Yield
-- fromLegacyYield (Legacy.Yield (Legacy.ObjectMap o) yprov ychain)
--   = do
--   o' <- traverse (\(k, v) -> (fromLegacyField k,) <$> fromLegacyPactValue v) (M.toList o)
--   pure $ Yield
--       (M.fromList o')
--       (fromLegacyProvenance <$> yprov)
--       (fromLegacyChainId <$> ychain)

-- fromLegacyField :: Legacy.FieldKey -> Field
-- fromLegacyField (Legacy.FieldKey f) = Field f

-- fromLegacyChainId :: Legacy.ChainId -> ChainId
-- fromLegacyChainId (Legacy.ChainId i) = ChainId i

-- fromLegacyProvenance :: Legacy.Provenance -> Provenance
-- fromLegacyProvenance (Legacy.Provenance tchain mh)
--   = Provenance (fromLegacyChainId tchain) (fromLegacyModuleHash mh)

-- fromLegacyModuleHash
--   :: Legacy.ModuleHash
--   -> ModuleHash
-- fromLegacyModuleHash (Legacy.ModuleHash h) = ModuleHash (fromLegacyHash h)

-- fromLegacyHash
--   :: Legacy.Hash
--   -> Hash
-- fromLegacyHash (Legacy.Hash h) = Hash h

-- decodeNamespace :: ByteString -> Maybe Namespace
-- decodeNamespace o = do
--   obj <- JD.decodeStrict' o
--   either (const Nothing) Just (fromLegacyNamespace obj)

-- fromLegacyNamespace
--   :: Legacy.Namespace Legacy.PactValue
--   -> Either String Namespace
-- fromLegacyNamespace (Legacy.Namespace ns u a) = do
--   let ns' = fromLegacyNamespaceName ns
--   u' <- fromLegacyGuard' u
--   a' <- fromLegacyGuard' a
--   pure (Namespace ns' u' a')

-- fromLegacyGuard'
--   :: Legacy.Guard Legacy.PactValue
--   -> Either String (Guard QualifiedName PactValue)
-- fromLegacyGuard' = \case
--   Legacy.GPact (Legacy.PactGuard i n) -> let
--     Legacy.PactId pid = i
--     in pure (GDefPactGuard (DefPactGuard (DefPactId pid) n))
--   Legacy.GKeySet ks -> GKeyset <$> fromLegacyKeySet ks
--   Legacy.GKeySetRef (Legacy.KeySetName ksn ns) -> let
--     ns' = fromLegacyNamespaceName <$>  ns
--     in pure (GKeySetRef $ KeySetName ksn ns')
--   Legacy.GCapability (Legacy.CapabilityGuard n a i) -> do
--     let qn = fromLegacyQualifiedName n
--     let pid = fmap fromLegacyPactId i
--     args <- traverse fromLegacyPactValue a
--     pure (GCapabilityGuard (CapabilityGuard qn args pid))

--   Legacy.GModule (Legacy.ModuleGuard mn n) -> let
--     mn' = fromLegacyModuleName mn
--     in pure (GModuleGuard (ModuleGuard mn' n))

--   Legacy.GUser (Legacy.UserGuard n a) -> case n of
--     Legacy.QName n' -> do
--       let qn = fromLegacyQualifiedName n'
--       args <- traverse fromLegacyPactValue a
--       pure (GUserGuard (UserGuard qn args))
--     _ -> error "todo: jose, other cases relevant?"

-- decodeRowData :: ByteString -> Maybe RowData
-- decodeRowData o = do
--   obj <- JD.decodeStrict' o
--   either (const Nothing) Just (fromLegacyRowData obj)

-- fromLegacyRowData
--   :: Legacy.RowData
--   -> Either String RowData
-- fromLegacyRowData (Legacy.RowData _ (Legacy.ObjectMap m)) = do
--   let f = fromLegacyPactValue .rowDataToPactValue
--   m' <- traverse (\(k,v) -> (fromLegacyField k,) <$> f v) (M.toList m)
--   pure (RowData (M.fromList m'))

-- rowDataToPactValue :: Legacy.RowDataValue -> Legacy.PactValue
-- rowDataToPactValue rdv = case rdv of
--   Legacy.RDLiteral l -> Legacy.PLiteral l
--   Legacy.RDList l -> Legacy.PList $ recur l
--   Legacy.RDObject o -> Legacy.PObject $ recur o
--   Legacy.RDGuard g -> Legacy.PGuard $ recur g
--   Legacy.RDModRef m -> Legacy.PModRef m
--   where
--     recur :: Functor f => f Legacy.RowDataValue -> f Legacy.PactValue
--     recur = fmap rowDataToPactValue
