{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# options_ghc -fno-warn-partial-fields #-}

module Chainweb.Pact.Backend.PactState.SizeInfo
  ( main
  )
  where

import Control.Monad (forM_, guard, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Attoparsec.Text qualified as AT
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Coerce (coerce)
import Data.Foldable qualified as F
import Data.HashMap.Strict qualified as HM
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Database.SQLite3.Direct (Utf8(..), Database)
import GHC.Stack (HasCallStack)
import Options.Applicative
import Prelude hiding (mod)
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S
import System.IO qualified as IO
import System.LogLevel qualified as LL
import System.Logger qualified
import UnliftIO.Async (pooledMapConcurrentlyN_)

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Logger (logFunctionText)
import Chainweb.Utils (toText)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName, ChainId, chainIdToText)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (fromUtf8, withSqliteDb)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (PactRow(..), TableType(..), identifyTableType, tableTypeToText, streamQry, checkpointerTables, compactionTables, pactSysTables)
import Chainweb.Pact.Backend.PactState.Utils (inQuotes, buildPrettyBytes, buildW64, bsLength, buildI64, doesPactDbExist, fromTextSilly, dontIncludeZero, dontIncludeZeroBytes)

import Pact.JSON.Decode qualified as JD
import Pact.JSON.Encode ((.=), (.?=))
import Pact.JSON.Encode qualified as J
import Pact.Types.Continuation (PactExec(..), Yield(..), PactContinuation(..), NestedPactExec(..), fromNestedPactExec)
import Pact.Types.Exp (Literal(..), Exp(..))
import Pact.Types.Info (Info(..), getInfo)
import Pact.Types.KeySet (keysetNameParser)
import Pact.Types.Names (ModuleName(..), NamespaceName(..), FullyQualifiedName(..), parseModuleName)
import Pact.Types.Namespace (Namespace(..))
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.Persistence (RowKey(..), PersistModuleData, ModuleData(..), PersistDirect(..))
import Pact.Types.RowData (RowDataVersion(..), RowData(..), RowDataValue(..))
import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact
import Pact.Types.Term (ObjectMap(..), Guard(..), ModRef(..), CapabilityGuard(..), KeySetName(..), KeySet(..), PactId(..), Ref'(..), Interface(..), Module(..), Def(..), ModuleDef(..), Governance(..), Term(..), Meta(..), DefMeta(..), DefcapMeta(..), Use(..), ModuleGuard(..), UserGuard(..))
import Pact.Types.Type (FunType(..), Arg(..))
import Pact.Types.Type qualified as Pact

getPactSizedTableNames :: (HasCallStack) => Database -> IO (Vector (Utf8, Word64))
getPactSizedTableNames db = do
  let sortedTableNames :: [[SType]] -> [(Utf8, Word64)]
      sortedTableNames rows =
        List.sortOn snd
        $ flip List.map rows $ \case
            [SText tbl, SInt tblSize] -> (tbl, fromIntegral @_ @Word64 tblSize)
            _ -> error "getPactTables.sortedTableNames: expected (text, int)"

  let qryText = mconcat
        [ "SELECT name, SUM(\"pgsize\") table_size "
        , "FROM \"dbstat\" "
        , "WHERE "
        , "  name NOT LIKE \"sqlite_%\" " -- filter out sqlite special tables
        , "GROUP BY name "
        , "ORDER BY table_size DESC"
        ]
  tables <- sortedTableNames <$> Pact.qry db qryText [] [RText, RInt]
  pure (Vector.fromList tables)

getPactSizedTables :: (HasCallStack)
  => [Utf8]
  -> Database
  -> Stream (Of SizedTable) IO ()
getPactSizedTables excludedTables db = do
  tables <- liftIO $ getPactSizedTableNames db
  forM_ tables $ \(tbl, sz) -> do
    when (tbl `notElem` excludedTables) $ do
      let tblType = identifyTableType tbl

      case tblType of
        Ix -> do
          S.yield $ SizedTable
            { name = fromUtf8 tbl
            , typ = tblType
            , size = sz
            , nullCounts = mempty
            , rows = []
            }
        Checkpointer -> do
          S.yield $ SizedTable
            { name = fromUtf8 tbl
            , typ = tblType
            , size = sz
            , nullCounts = mempty
            , rows = []
            }
        Compaction -> do
          error "Compaction tables are not supported"
        _ -> do
          let qryText = "SELECT rowkey, rowdata, txid FROM " <> inQuotes tbl
          sizedRows <- liftIO $ streamQry db qryText [] [RText, RBlob, RInt] $ \rows -> do
            S.toList_
              $ flip S.map rows $ \case
                  [SText (Utf8 rowKey), SBlob rowData, SInt txId] ->
                    sizeTagPactRow (identifyDomain tbl) $
                      PactRow {..}
                  _ -> error "getPactSizedTables: expected (text, blob, int)"
          S.yield $ SizedTable
            { name = fromUtf8 tbl
            , typ = identifyTableType tbl
            , size = sz
            , nullCounts = flip F.foldMap' sizedRows $ \case
                SizedUserRow {..} -> rowData.nullCounts
                SizedPactsRow {..} -> case pactExec of
                  Just pe -> pe.nullCounts
                  Nothing -> NullCounts (M.singleton "pact_exec" 1)
                SizedModuleRow {..} -> componentNullCounts moduleName <> moduleData.nullCounts
                SizedNamespaceRow {..} -> namespace.nullCounts
                SizedKeySetRow {..} -> keySet.nullCounts
            , rows = sizedRows
            }

sumWith :: (Foldable t, Num a) => t x -> (x -> a) -> a
sumWith t f = F.foldl' (\acc x -> acc + f x) 0 t

jsonSize :: (J.Encode a) => a -> Word64
jsonSize x = fromIntegral (BS.length (J.encodeStrict x))

newtype TableTypeSizes = TableTypeSizes (Map TableType Word64)
  deriving newtype (Eq, Ord, Show)

instance Semigroup TableTypeSizes where
  TableTypeSizes m1 <> TableTypeSizes m2 = TableTypeSizes (M.unionWith (+) m1 m2)

instance Monoid TableTypeSizes where
  mempty = TableTypeSizes M.empty

encodeTableTypeSizes :: TableTypeSizes -> Maybe J.KeyValue
encodeTableTypeSizes (TableTypeSizes t) = ("table_type_sizes" .?=) $ do
  guard (any (> 0) t)
  pure $ J.Object $ flip List.map (M.toList t) $ \(typ, count) ->
    tableTypeToText typ .?= dontIncludeZeroBytes count

newtype NullCounts = NullCounts (Map Text Word64)
  deriving newtype (Eq, Ord, Show)

instance Semigroup NullCounts where
  NullCounts m1 <> NullCounts m2 = NullCounts (M.unionWith (+) m1 m2)

instance Monoid NullCounts where
  mempty = NullCounts M.empty

encodeNullCounts :: NullCounts -> Maybe J.KeyValue
encodeNullCounts (NullCounts n) = ("null_counts" .?=) $ do
  guard (any (> 0) n)
  pure $ J.Object $ flip List.map (M.toList n) $ \(cs, count) ->
    cs .?= dontIncludeZero count

data SizedRowDataValue
  = SRDLiteral Word64 Literal
  | SRDList Word64 NullCounts (Vector SizedRowDataValue)
  | SRDObject Word64 NullCounts (ObjectMap SizedRowDataValue)
  | SRDGuard Word64 NullCounts (Guard SizedRowDataValue)
  | SRDModRef Word64 NullCounts ModRef
  deriving stock (Eq, Ord, Show)

sizeTagRowDataValue :: RowDataValue -> SizedRowDataValue
sizeTagRowDataValue = go
  where
    go :: RowDataValue -> SizedRowDataValue
    go rdv =
      let
        topLevelSize = jsonSize rdv
      in
      case rdv of
        RDLiteral l -> SRDLiteral topLevelSize l
        RDList ls -> SRDList topLevelSize (rdvCountNulls rdv) (recur ls)
        RDObject o -> SRDObject topLevelSize (rdvCountNulls rdv) (recur o)
        RDGuard g -> SRDGuard topLevelSize (rdvCountNulls rdv) (recur g)
        RDModRef m -> SRDModRef topLevelSize (rdvCountNulls rdv) m

    recur :: (Functor f) => f RowDataValue -> f SizedRowDataValue
    recur = fmap go

rdvCountNulls :: RowDataValue -> NullCounts
rdvCountNulls = \case
  RDLiteral _ -> mempty
  RDList ls -> F.foldMap' rdvCountNulls ls
  RDObject o -> F.foldMap' rdvCountNulls o
  RDGuard g -> case g of
    GCapability cg -> componentNullCounts cg
    _ -> mempty
  RDModRef m -> componentNullCounts m

instance J.Encode SizedRowDataValue where
  build = \case
    SRDLiteral sz l -> J.object
      [ "size" .= buildPrettyBytes sz
      , "literal" .= l
      ]
    SRDList sz nullCount v -> J.object
      [ "size" .= buildPrettyBytes sz
      , encodeNullCounts nullCount
      , "list" .= J.array v
      ]
    SRDObject sz nullCount o -> J.object
      [ "size" .= buildPrettyBytes sz
      , encodeNullCounts nullCount
      , "object" .= o
      ]
    SRDGuard sz nullCount g -> J.object
      [ "size" .= buildPrettyBytes sz
      , encodeNullCounts nullCount
      , "guard" .= g
      ]
    SRDModRef sz nullCount (ModRef refName refSpec _) -> J.object
      [ "size" .= buildPrettyBytes sz
      , encodeNullCounts nullCount
      , "mod_ref" .= J.object
          [ "ref_spec" .= fmap J.array refSpec
          , "ref_name" .= refName
          ]
      ]

rowDataNullCount :: SizedRowDataValue -> NullCounts
rowDataNullCount = \case
  SRDLiteral _ _ -> mempty
  SRDList _ n _ -> n
  SRDObject _ n _ -> n
  SRDGuard _ n _ -> n
  SRDModRef _ n _ -> n

data SizedRowData = SizedRowData
  { size :: Word64
  , nullCounts :: NullCounts
  , rdVersion :: RowDataVersion
  , rdData :: ObjectMap SizedRowDataValue
  }
  deriving stock (Eq, Ord, Show)

sizeTagRowData :: Word64 -> RowData -> SizedRowData
sizeTagRowData size rd =
  let rdData = fmap sizeTagRowDataValue rd._rdData
      nullCounts = F.foldMap' rowDataNullCount rdData
  in
  SizedRowData
    { size = size
    , nullCounts = nullCounts
    , rdVersion = rd._rdVersion
    , rdData = rdData
    }

instance J.Encode SizedRowData where
  build srd = J.object
    [ "size" .= buildPrettyBytes srd.size
    , encodeNullCounts srd.nullCounts
    , "sized_data" .= srd.rdData
    , "version" .= srd.rdVersion
    ]

data ComponentTree' tag
  = Leaf tag Word64
  | Node tag Word64 [ComponentTree' tag]
  deriving stock (Eq, Show)
  deriving stock (Functor, Foldable, Traversable)

type ComponentTree = ComponentTree' Text

componentSize :: ComponentTree' tag -> Word64
componentSize = \case
  Leaf _ sz -> sz
  Node _ sz _ -> sz

buildNode :: tag -> [ComponentTree' tag] -> ComponentTree' tag
buildNode tag subcomponents = Node tag (sumWith subcomponents componentSize) subcomponents

nullableLeaf :: tag -> Maybe a -> ComponentTree' tag
nullableLeaf tag m = Leaf tag (countNothing m)

overLeaf :: tag -> (a -> ComponentTree' tag) -> Maybe a -> ComponentTree' tag
overLeaf tag f = \case
  Nothing -> Leaf tag 1
  Just a -> f a

-- Only cares about leaf nodes
-- Because that's where all the interesting values are
flattenComponents :: ComponentTree -> NullCounts
flattenComponents = go
  where
    go = \case
      Leaf tag sz -> NullCounts (M.singleton tag sz)
      Node _ _ subcomponents -> foldMap go subcomponents

componentNullCounts :: (Component a) => a -> NullCounts
componentNullCounts a = flattenComponents (component a)

class Component a where
  component :: a -> ComponentTree

instance (Component g, CountNulls g) => Component (Module g) where
  component m = buildNode "module"
    [ component m._mName
    , component m._mGovernance
    , component m._mMeta
    , buildNode "interfaces" (List.map component m._mInterfaces)
    , buildNode "imports" (List.map component m._mImports)
    ]

instance Component Interface where
  component i = buildNode "interface"
    [ component i._interfaceName
    , component i._interfaceMeta
    , buildNode "imports" (List.map component i._interfaceImports)
    ]

instance (Component n, CountNulls n) => Component (Def n) where
  component d = buildNode "def"
    [ component d._dModule
    , component d._dFunType
    , Leaf "def_body" (sumWith d._dDefBody countNulls)
    , component d._dMeta
    , case d._dDefMeta of
        Nothing -> Leaf "meta" 1
        Just dfm -> component dfm
    , component d._dInfo
    ]

instance Component ModuleName where
  component mn = Leaf "module_namespace" (countNulls mn)

instance (Component g, CountNulls g) => Component (Governance g) where
  component gv@(Governance g) = Node "governance" (countNulls gv) $ case g of
    Right x -> [component x]
    Left _ -> []

-- | `Terms` are hugely nested; hence they are leaves
--   (i.e, we don't recurse and build a tree out of them)
instance (CountNulls n) => Component (Term n) where
  component t = Leaf "term" (countNulls t)

instance (Component d, CountNulls d) => Component (Ref' d) where
  component r = Leaf "ref'" (countNulls r) -- should be Node

instance Component PersistDirect where
  component p = buildNode "persist_direct" $ case p of
    PDValue pv -> [component pv]
    PDNative _ -> []
    PDFreeVar _ -> []

instance Component PactValue where
  component pv = Leaf "pact_value" (countNulls pv)

instance (Component o, CountNulls o) => Component (FunType o) where
  component f = buildNode "fun_type"
    [ buildNode "args" (List.map component f._ftArgs)
    , component f._ftReturn
    ]

instance (Component o, CountNulls o) => Component (Arg o) where
  component a = buildNode "arg"
    [ component a._aType
    , component a._aInfo
    ]

instance (Component o, CountNulls o) => Component (Pact.Type o) where
  component t = Leaf "type" (countNulls t)

instance Component Info where
  component i = Leaf "info" (countNulls i)

instance Component Meta where
  component m = buildNode "meta"
    [ Leaf "docs" (countNothing m._mDocs)
    , buildNode "model" (List.map component m._mModel)
    ]

instance (Component n, CountNulls n) => Component (DefMeta n) where
  component d = Leaf "def_meta" (countNulls d)

instance Component (Exp Info) where
  component e = Leaf "info" (countNulls e)

instance Component Use where
  component u = buildNode "use"
    [ component u._uModuleName
    , nullableLeaf "module_hash" u._uModuleHash
    , nullableLeaf "imports" u._uImports
    , component u._uInfo
    ]

instance Component PactExec where
  component pe = buildNode "pact_exec"
    [ overLeaf "yield" component pe._peYield
    , nullableLeaf "bool" pe._peExecuted
    , component pe._peContinuation
    , buildNode "nested_pact_execs" (List.map (component . snd) (M.toList pe._peNested))
    ]

instance Component Yield where
  component y = buildNode "yield"
    [ Leaf "pact_value" (countNulls y._yData)
    , Leaf "chain_id" (countNothing y._ySourceChain)
    ]

instance Component PactContinuation where
  component pc = buildNode "pact_execution"
    [ buildNode "args" (List.map component pc._pcArgs)
    ]

instance Component NestedPactExec where
  -- the boolean passed to `fromNestedPactExec` has no bearing on its
  -- null count
  component npe = case component (fromNestedPactExec False npe) of
    Node _ sz t -> Node "nested_pact_exec" sz t
    _ -> error "PactExec: component was a Leaf node. this should be impossible"

instance (Component a) => Component (Namespace a) where
  component n = buildNode "namespace"
    [ component n._nsUser
    , component n._nsAdmin
    ]

instance (Component a) => Component (CapabilityGuard a) where
  component cg = buildNode "capability_guard"
    [ buildNode "args" (List.map component cg._cgArgs)
    ]

instance Component ModuleGuard where
  component mg = buildNode "module_guard"
    [ component mg._mgModuleName
    ]

instance (Component a) => Component (UserGuard a) where
  component ug = buildNode "user_guard"
    [ buildNode "args" (List.map component ug._ugArgs)
    ]

instance (Component a) => Component (Guard a) where
  component g = buildNode "guard" $ case g of
    GPact _ -> []
    GKeySet _ -> []
    GKeySetRef _ -> []
    GModule mg -> [component mg]
    GUser ug -> [component ug]
    GCapability cg -> [component cg]

instance Component RowDataValue where
  component rdv = Leaf "row_data_value" (countNulls rdv)

instance Component ModRef where
  component m = buildNode "mod_ref"
    [ Leaf "module_namespace" $ case m._modRefSpec of
        Nothing -> 1
        Just modNames -> sumWith modNames (\mn -> countNothing mn._mnNamespace)
    , Leaf "info" (countNothing m._modRefInfo._iInfo)
    ]

class CountNulls a where
  countNulls :: a -> Word64

instance CountNulls ModuleName where
  countNulls mn = countNothing mn._mnNamespace

instance (CountNulls g) => CountNulls (Governance g) where
  countNulls (Governance e) = case e of
    Right g -> countNulls g
    _ -> 0

instance (CountNulls g) => CountNulls (Module g) where
  countNulls m =
    countNulls m._mName
    + countNulls m._mGovernance
    + countNulls m._mMeta
    + countNulls m._mInterfaces
    + countNulls m._mImports

instance CountNulls Interface where
  countNulls i =
    countNulls i._interfaceName
    + countNulls i._interfaceMeta
    + countNulls i._interfaceImports

instance (CountNulls n) => CountNulls (Def n) where
  countNulls d =
    countNulls d._dModule
    + countNulls d._dFunType
    + sumWith d._dDefBody countNulls
    + countNulls d._dMeta
    + countNulls d._dDefMeta
    + countNulls (getInfo d)

instance (CountNulls n) => CountNulls (Term n) where
  countNulls t =
    sumWith t countNulls
    + countNulls (getInfo t)

instance (CountNulls d) => CountNulls (Ref' d) where
  countNulls = \case
    Direct d -> countNulls d
    Ref term -> countNulls term

instance CountNulls PersistDirect where
  countNulls = \case
    PDValue pv -> countNulls pv
    PDNative _ -> 0
    PDFreeVar _ -> 0

instance CountNulls PactValue where
  countNulls = \case
    PLiteral _ -> 0
    PList ls -> sumWith ls countNulls
    PObject o -> sumWith o countNulls
    PGuard g -> sumWith g countNulls
    PModRef mr ->
      countNulls mr._modRefName
      + countNulls mr._modRefSpec
      + countNulls mr._modRefInfo

instance (CountNulls o) => CountNulls (FunType o) where
  countNulls f = sumWith f._ftArgs countNulls + countNulls f._ftReturn

instance (CountNulls o) => CountNulls (Arg o) where
  countNulls a = countNulls a._aType + countNulls a._aInfo

instance (CountNulls o) => CountNulls (Pact.Type o) where
  countNulls t = sumWith t countNulls

instance CountNulls Meta where
  countNulls m = countNothing m._mDocs + countNulls m._mModel

instance (CountNulls n) => CountNulls (DefMeta n) where
  countNulls (DMDefcap d) = countNulls d

instance (CountNulls n) => CountNulls (DefcapMeta n) where
  countNulls = \case
    DefcapEvent -> 0
    DefcapManaged m -> countNulls (fmap snd m)

instance (CountNulls i) => CountNulls (Exp i) where
  countNulls e = sumWith e countNulls

instance CountNulls Info where
  countNulls (Info i) = countNothing i

instance (CountNulls a) => CountNulls [a] where
  countNulls ls = sumWith ls countNulls

instance (CountNulls a) => CountNulls (Maybe a) where
  countNulls = \case
    Nothing -> 1
    Just a -> countNulls a

instance CountNulls Use where
  countNulls u =
    countNulls u._uModuleName
    + countNothing u._uModuleHash
    + countNothing u._uImports
    + countNulls u._uInfo

instance CountNulls PactExec where
  countNulls pe =
    countNulls pe._peYield
    + countNothing pe._peExecuted
    + countNulls pe._peContinuation
    + sumWith pe._peNested countNulls

instance CountNulls Yield where
  countNulls y =
    countNulls y._yData
    + countNothing y._ySourceChain

instance (CountNulls a) => CountNulls (ObjectMap a) where
  countNulls o = sumWith o countNulls

instance CountNulls PactContinuation where
  countNulls pc = countNulls pc._pcArgs

instance CountNulls NestedPactExec where
  countNulls npe =
    countNulls npe._npeYield
    + countNothing npe._npeExecuted
    + countNulls npe._npeContinuation
    + sumWith npe._npeNested countNulls

instance CountNulls RowDataValue where
  countNulls rdv = sum (M.elems (coerce (rdvCountNulls rdv)))

countNothing :: Maybe a -> Word64
countNothing m = if isNothing m then 1 else 0

-- | What the 'r' type variable is instantiated to
--   when the pact values are stored in the database.
type R = Ref' PersistDirect

data SizedModuleDef
  = SizedModule Word64 NullCounts (Module (Def R))
  | SizedInterface Word64 NullCounts Interface

instance J.Encode SizedModuleDef where
  build = \case
    SizedModule size nullCounts _ -> J.object
      [ "tag" .= J.text "sized_module"
      , "size" .= buildPrettyBytes size
      , encodeNullCounts nullCounts
      ]
    SizedInterface size nullCounts _ -> J.object
      [ "tag" .= J.text "sized_interface"
      , "size" .= buildPrettyBytes size
      , encodeNullCounts nullCounts
      ]

sizeTagModuleDef :: Word64 -> ModuleDef (Def R) -> SizedModuleDef
sizeTagModuleDef size = \case
  MDModule mod -> SizedModule size (componentNullCounts mod) mod
  MDInterface iface -> SizedInterface size (componentNullCounts iface) iface

data SizedModuleData = SizedModuleData
  { size :: Word64
  , nullCounts :: NullCounts
  , mod :: SizedModuleDef
  , refMap :: HM.HashMap Text R -- TODO: R can contain nulls
  , dependencies :: HM.HashMap FullyQualifiedName R -- TODO: R can contain nulls
  }

instance J.Encode SizedModuleData where
  build smd = J.object
    [ "size" .= buildPrettyBytes smd.size
    , encodeNullCounts smd.nullCounts
    , "module_def" .= smd.mod
    ]

sizeTagModuleData :: Word64 -> PersistModuleData -> SizedModuleData
sizeTagModuleData size modData =
  let
    sizedModuleDef = sizeTagModuleDef (jsonSize modData._mdModule) modData._mdModule
  in
  SizedModuleData
    { size = size
    , nullCounts = case sizedModuleDef of
        SizedModule _ n _ -> n
        SizedInterface _ n _ -> n
    , mod = sizedModuleDef
    , refMap = modData._mdRefMap
    , dependencies = modData._mdDependencies
    }

data SizedPactExec = SizedPactExec
  { size :: Word64
  , nullCounts :: NullCounts
  , pactExec :: PactExec
  }

instance J.Encode SizedPactExec where
  build spe = J.object
    [ "size" .= buildW64 spe.size
    , encodeNullCounts spe.nullCounts
    , "pact_exec" .= spe.pactExec
    ]

sizeTagPactExec :: Word64 -> PactExec -> SizedPactExec
sizeTagPactExec sz pe = SizedPactExec
  { size = sz
  , nullCounts = componentNullCounts pe
  , pactExec = pe
  }

data SizedNamespace = SizedNamespace
  { size :: Word64
  , nullCounts :: NullCounts
  , namespace :: Namespace PactValue
  }

instance J.Encode SizedNamespace where
  build sn = J.object
    [ "size" .= buildW64 sn.size
    , encodeNullCounts sn.nullCounts
    , "namespace" .= sn.namespace
    ]

sizeTagNamespace :: Word64 -> Namespace PactValue -> SizedNamespace
sizeTagNamespace sz ns = SizedNamespace
  { size = sz
  , nullCounts = componentNullCounts ns
  , namespace = ns
  }

data SizedKeySet = SizedKeySet
  { size :: Word64
  , nullCounts :: NullCounts
  , keySet :: KeySet
  }

instance J.Encode SizedKeySet where
  build sks = J.object
    [ "size" .= buildW64 sks.size
    , encodeNullCounts sks.nullCounts
    , "key_set" .= sks.keySet
    ]

sizeTagKeySet :: Word64 -> KeySet -> SizedKeySet
sizeTagKeySet sz ks = SizedKeySet
  { size = sz
  , nullCounts = mempty
  , keySet = ks
  }

data SizedPactRow where
  SizedUserRow ::
    { size   :: Word64
    , rowKey :: RowKey
    , rowData :: SizedRowData
    , txId :: Int64
    } -> SizedPactRow
  SizedPactsRow ::
    { size   :: Word64
    , pactId :: PactId
    , pactExec :: Maybe SizedPactExec
    , txId :: Int64
    } -> SizedPactRow
  SizedModuleRow ::
    { size :: Word64
    , moduleName :: ModuleName
    , moduleData :: SizedModuleData
    , txId :: Int64
    } -> SizedPactRow
  SizedNamespaceRow ::
    { size :: Word64
    , namespaceName :: NamespaceName
    , namespace :: SizedNamespace
    , txId :: Int64
    } -> SizedPactRow
  SizedKeySetRow ::
    { size :: Word64
    , keySetName :: KeySetName
    , keySet :: SizedKeySet
    , txId :: Int64
    } -> SizedPactRow

sizeTagPactRow :: Some1 Domain -> PactRow -> SizedPactRow
sizeTagPactRow domain pr = case domain of
  Some1 UserTables -> case decodeDomain UserTables rkText pr.rowData of
    (rk, rd) ->
      SizedUserRow
        { size = len
        , rowKey = rk
        , rowData = sizeTagRowData rdLen rd
        , txId = pr.txId
        }
  Some1 KeySets -> case decodeDomain KeySets rkText pr.rowData of
    (ksName, ks) ->
      SizedKeySetRow
        { size = len
        , keySetName = ksName
        , keySet = sizeTagKeySet rdLen ks
        , txId = pr.txId
        }
  Some1 Modules -> case decodeDomain Modules rkText pr.rowData of
    (modName, modData) ->
      SizedModuleRow
        { size = len
        , moduleName = modName
        , moduleData = sizeTagModuleData rdLen modData
        , txId = pr.txId
        }
  Some1 Namespaces -> case decodeDomain Namespaces rkText pr.rowData of
    (nsName, ns) ->
      SizedNamespaceRow
        { size = len
        , namespaceName = nsName
        , namespace = sizeTagNamespace rdLen ns
        , txId = pr.txId
        }
  Some1 Pacts -> case decodeDomain Pacts rkText pr.rowData of
    (pactId, pactExec) ->
      SizedPactsRow
        { size = len
        , pactId = pactId
        , pactExec = sizeTagPactExec rdLen <$> pactExec
        , txId = pr.txId
        }
  where
    rdLen = bsLength pr.rowData
    len = bsLength pr.rowKey + rdLen
    rkText = Text.decodeUtf8 pr.rowKey

instance J.Encode SizedPactRow where
  build = \case
    SizedUserRow {..} -> J.object
      [ "tag" .= J.text "user"
      , "size" .= buildPrettyBytes size
      , "row_key" .= J.text (coerce rowKey)
      , "sized_row_data" .= rowData
      , "tx_id" .= buildI64 txId
      ]
    SizedPactsRow {..} -> J.object
      [ "tag" .= J.text "pact"
      , "size" .= buildPrettyBytes size
      , "pact_id" .= J.text (coerce pactId)
      , "sized_pact_exec" .?= pactExec
      , "tx_id" .= buildI64 txId
      ]
    SizedModuleRow {..} -> J.object
      [ "tag" .= J.text "module"
      , "size" .= buildPrettyBytes size
      , "module_name" .= moduleName
      , "sized_module_data" .= moduleData
      , "tx_id" .= buildI64 txId
      ]
    SizedNamespaceRow {..} -> J.object
      [ "tag" .= J.text "namespace"
      , "size" .= buildPrettyBytes size
      , "namespace_name" .= namespaceName
      , "sized_namespace" .= namespace
      , "tx_id" .= buildI64 txId
      ]
    SizedKeySetRow {..} -> J.object
      [ "tag" .= J.text "key_set"
      , "size" .= buildPrettyBytes size
      , "key_set_name" .= keySetName
      , "sized_key_set" .= keySet
      , "tx_id" .= buildI64 txId
      ]

data SizedTable = SizedTable
  { name :: Text
  , typ :: TableType
  , size :: Word64
  , nullCounts :: NullCounts
  , rows :: [SizedPactRow]
  }

instance J.Encode SizedTable where
  build tbl = J.object
    [ "name" .= tbl.name
    , "type" .= tbl.typ
    , "size" .= buildPrettyBytes tbl.size
    , encodeNullCounts tbl.nullCounts
    , encodeList tbl.rows
    ]
    where
      encodeList :: (J.Encode a) => [a] -> Maybe J.KeyValue
      encodeList = \case
        [] -> Nothing
        ls -> "sized_rows" .= J.array ls

data ChainSizeInfo = ChainSizeInfo
  { size :: Word64
  , nullCounts :: NullCounts
  , tableTypeSizes :: TableTypeSizes
  , sizedTables :: Vector SizedTable
  }

instance J.Encode ChainSizeInfo where
  build c = J.object
    [ "size" .= buildPrettyBytes c.size
    , encodeNullCounts c.nullCounts
    , encodeTableTypeSizes c.tableTypeSizes
    , "sized_tables" .= J.array c.sizedTables
    ]

mkChainSizeInfo :: Vector SizedTable -> ChainSizeInfo
mkChainSizeInfo tbls =
  let
    (size, nullCounts, tableTypeSizes) = Vector.foldl'
      (\(!szAcc, !nullCountsAcc, !tableTypeSizesAcc) tbl -> (szAcc + tbl.size, nullCountsAcc <> tbl.nullCounts, tableTypeSizesAcc <> TableTypeSizes (M.singleton tbl.typ tbl.size)))
      (0, mempty, mempty)
      tbls
  in
  ChainSizeInfo
    { size = size
    , nullCounts = nullCounts
    , sizedTables = tbls
    , tableTypeSizes = tableTypeSizes
    }

data PactBreakdown = PactBreakdown
  { size :: Word64
  , nullCounts :: NullCounts
  , tableTypeSizes :: TableTypeSizes
  , sizedChains :: Map ChainId ChainSizeInfo
  }

instance J.Encode PactBreakdown where
  build b = J.object
    [ "size" .= buildPrettyBytes b.size
    , encodeNullCounts b.nullCounts
    , encodeTableTypeSizes b.tableTypeSizes
    , "sized_chains" .= J.Object (List.map (first chainIdToText) (M.toList b.sizedChains))
    ]

mkPactBreakdown :: Map ChainId ChainSizeInfo -> PactBreakdown
mkPactBreakdown sizedChains =
  let
    (size, nullCounts, tableTypeSizes) = M.foldl'
      (\(!szAcc, !nullCountsAcc, !tableTypeSizesAcc) sc -> (szAcc + sc.size, nullCountsAcc <> sc.nullCounts, tableTypeSizesAcc <> sc.tableTypeSizes)
      )
      (0, mempty, mempty)
      sizedChains
  in
  PactBreakdown
    { size = size
    , nullCounts = nullCounts
    , tableTypeSizes = tableTypeSizes
    , sizedChains = sizedChains
    }

data PactBreakdownConfig = PactBreakdownConfig
  { pactDbDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , numThreads :: Int
  , excludeSystemTables :: Bool
  }

main :: IO ()
main = do
  cfg <- execParser opts

  let cids = List.sort $ F.toList $ chainIdsAt cfg.chainwebVersion (BlockHeight maxBound)

  sizesRef <- newIORef @(Map ChainId (Vector SizedTable)) M.empty

  flip (pooledMapConcurrentlyN_ cfg.numThreads) cids $ \cid -> do
    C.withDefaultLogger System.Logger.Error $ \logger -> do
      let logText = logFunctionText logger
      let resetDb = False

      sqliteFileExists <- doesPactDbExist cid cfg.pactDbDir
      if sqliteFileExists
      then do
        withSqliteDb cid logger cfg.pactDbDir resetDb $ \(SQLiteEnv db _) -> do
          let excludedTables = concat
                [ checkpointerTables
                , compactionTables
                , if cfg.excludeSystemTables then pactSysTables else []
                ]
          e <- S.next (getPactSizedTables excludedTables db)
          case e of
            Left () -> do
              -- the stream was entirely empty, but we need an entry
              atomicModifyIORef' sizesRef $ \m -> (M.insert cid Vector.empty m, ())
            Right (tbl, rest) -> do
              sizedTables <- (tbl :) <$> S.toList_ rest
              atomicModifyIORef' sizesRef $ \m -> (M.insert cid (Vector.fromList sizedTables) m, ())
      else do
        logText LL.Warn $ "[SQLite for chain in " <> Text.pack cfg.pactDbDir <> " doesn't exist. Skipping]"
        -- There was nothing, but we need an entry.
        atomicModifyIORef' sizesRef $ \m -> (M.insert cid Vector.empty m, ())

  sizes <- readIORef sizesRef
  let chainSizeInfos = M.map mkChainSizeInfo sizes
  let breakdown = mkPactBreakdown chainSizeInfos
  BSL8.hPut IO.stdout (J.encode breakdown)

  where
    opts :: ParserInfo PactBreakdownConfig
    opts = info (parser <**> helper)
      (fullDesc <> progDesc "Pact DB breakdown")

    parser :: Parser PactBreakdownConfig
    parser = PactBreakdownConfig
      <$> strOption
           (long "pact-database-dir"
            <> metavar "PACT_DB_DIRECTORY"
            <> help "Pact database directory")
      <*> (fmap (lookupVersionByName . fromTextSilly @ChainwebVersionName) $ strOption
           (long "graph-version"
            <> metavar "CHAINWEB_VERSION"
            <> help "Chainweb version for graph. Only needed for non-standard graphs."
            <> value (toText (_versionName mainnet))
            <> showDefault))
      <*> option auto
           (long "threads"
            <> metavar "NUM_THREADS"
            <> help "Number of threads on which to run compaction."
            <> value 4)
      <*> switch
            (long "exclude-pact-system-tables"
             <> help "Whether or not to exclude Pact System tables."
             <> showDefault)

data Domain k v where
  UserTables :: Domain RowKey RowData
  KeySets :: Domain KeySetName KeySet
  Modules :: Domain ModuleName PersistModuleData
  Namespaces :: Domain NamespaceName (Namespace PactValue)
  Pacts :: Domain PactId (Maybe PactExec)

decodeDomain :: (HasCallStack) => forall k v. Domain k v -> Text -> ByteString -> (k, v)
decodeDomain d keyBytes valBytes = case go of
  Left err -> error err
  Right x -> x
  where
    eitherDecode :: (JD.FromJSON a) => String -> ByteString -> Either String a
    eitherDecode msg bytes = case JD.eitherDecodeStrict' bytes of
      Right a -> Right a
      Left err -> Left (msg ++ ": " ++ err)

    -- Why are they stored like this? Who knows!
    parsePactId :: Text -> PactId
    parsePactId t
      | t == "" = PactId ""
      | "PactId \"" `Text.isPrefixOf` t && Text.last t == '\"' =
          PactId (Text.dropEnd 1 (Text.drop (Text.length "PactId \"") t))
      | otherwise = PactId t

    go = case d of
      UserTables -> do
        let rk = RowKey keyBytes
        rd <- eitherDecode @RowData "RowData" valBytes
        pure (rk, rd)
      KeySets -> do
        ksName <- AT.parseOnly keysetNameParser keyBytes
        ks <- eitherDecode @KeySet "KeySet" valBytes
        pure (ksName, ks)
      Modules -> do
        mn <- parseModuleName keyBytes
        md <- eitherDecode @PersistModuleData "PersistModuleData" valBytes
        pure (mn, md)
      Namespaces -> do
        let nsn = NamespaceName keyBytes
        ns <- eitherDecode @(Namespace PactValue) "Namespace PactValue" valBytes
        pure (nsn, ns)
      Pacts -> do
        let pactId = parsePactId keyBytes
        pactExec <- JD.eitherDecodeStrict' @(Maybe PactExec) valBytes
        pure (pactId, pactExec)

type Some1 :: (Type -> Type -> Type) -> Type
data Some1 f where
  Some1 :: f a b -> Some1 f

identifyDomain :: (HasCallStack) => Utf8 -> Some1 Domain
identifyDomain tbl = case identifyTableType tbl of
  User -> Some1 UserTables
  PactSys
    | tbl == "SYS:KeySets" -> Some1 KeySets
    | tbl == "SYS:Modules" -> Some1 Modules
    | tbl == "SYS:Namespaces" -> Some1 Namespaces
    | tbl == "SYS:Pacts" -> Some1 Pacts
  _ -> error "identifyDomain: non-pact table"
