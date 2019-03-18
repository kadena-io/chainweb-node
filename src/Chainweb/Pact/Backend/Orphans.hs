{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Pact.Backend.Orphans
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb
module Chainweb.Pact.Backend.Orphans where


import Control.Monad

import Data.Aeson hiding (Object)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Decimal
import Data.Default
import Data.Hashable
import Data.HashMap.Strict
import Data.HashSet
import Data.List.NonEmpty
import Data.Serialize hiding (getWord8, putWord8)
import Data.Thyme.Clock
import Data.Thyme.Internal.Micro
import qualified Data.Vector as Vector

import GHC.Generics hiding (Meta)

import Text.Trifecta.Delta

import Pact.Native
import Pact.Persist
import Pact.Persist.SQLite
import Pact.Types.Persistence
import Pact.Types.Runtime
import Pact.Types.Server

-----------------------
-- GENERIC INSTANCES --
-----------------------
deriving instance Generic ModuleData -- transferred

deriving instance Generic (ModuleDef a)

deriving instance Generic Ref -- transferred

deriving instance Generic RefStore -- transferred

deriving instance Generic CommandState -- transferred

deriving instance Generic DefName -- transferred

deriving instance Generic Decimal

deriving instance Generic Guard

deriving instance Generic Example

deriving instance Generic TableName

deriving instance Generic NativeDefName

deriving instance Generic (App (Term n))

deriving instance Generic (BindType (Type (Term n)))

deriving instance Generic (Term n)

deriving instance Generic (Def n)

deriving instance Generic (Governance g)

deriving instance Generic (Module g)

deriving instance Generic Interface

deriving instance Generic KeySetName

deriving instance Generic Use

deriving instance Generic TableId

deriving instance Generic Pragma

deriving instance Generic (Object n)

deriving instance Generic FieldKey

deriving instance Generic PactExec

deriving instance Generic PactContinuation

----------------------
-- SERIAL INSTANCES --
----------------------

deriving instance Serial TxId

deriving instance Serial ModuleData

deriving instance Serial Interface

deriving instance (Generic a, Serial a) => Serial (ModuleDef a)

deriving instance Serial Ref

instance Serial RefStore where
    serialize RefStore {..} = serialize _rsModules
    deserialize = do
        let _rsNatives = nativeDefs
        _rsModules <- deserialize
        return $ RefStore {..}

deriving instance Serial CommandState

deriving instance Serial Code

deriving instance Serial Parsed

deriving instance Serial Delta

deriving instance Serial Info

deriving instance Serial DefName

deriving instance Serial Example

deriving instance Serial ModuleName

deriving instance Serial NamespaceName

deriving instance Serial DefType

deriving instance Serial Meta

deriving instance Serial (Exp Info)

deriving instance Serial (LiteralExp Info)

deriving instance Serial (AtomExp Info)

deriving instance Serial (ListExp Info)

deriving instance Serial ListDelimiter

deriving instance Serial (SeparatorExp Info)

deriving instance Serial Separator

deriving instance Serial Literal

deriving instance Serial UTCTime

deriving instance Serial NominalDiffTime

deriving instance Serial Micro

deriving instance Serial Decimal

deriving instance (Generic n, Serial n) => Serial (Object n)

deriving instance Serial FieldKey

deriving instance Serial PactExec

deriving instance Serial PactContinuation


instance Serial1 Governance where
  serializeWith f (Governance t) = case t of
    Left r  -> putWord8 0 >> serialize r
    Right a -> putWord8 1 >> f a

  deserializeWith m = Governance <$> go
    where
      go = getWord8 >>= \a ->
        case a of
          0 -> Left <$> deserialize
          1 -> Right <$> m
          _ -> fail "Governance: Deserialization error."

instance Serial1 Module where
  serializeWith f Module{..} = do
    serialize _mName
    serializeWith f _mGovernance
    serialize _mMeta
    serialize _mCode
    serialize _mHash
    serialize _mBlessed
    serialize _mInterfaces
    serialize _mImports

  deserializeWith m = Module
    <$> deserialize       -- name
    <*> deserializeWith m -- gov
    <*> deserialize       -- meta
    <*> deserialize       -- code
    <*> deserialize       -- hash
    <*> deserialize       -- blessed
    <*> deserialize       -- interfaces
    <*> deserialize       -- imports

instance Serial1 ModuleDef where
    serializeWith f t = case t of
      MDModule m -> putWord8 0 >> serializeWith f m
      MDInterface i -> putWord8 1 >> serialize i

    deserializeWith m = getWord8 >>= \a ->
      case a of
        0 -> MDModule <$> deserializeWith m
        1 -> MDInterface <$> deserialize
        _ -> fail "ModuleDef: Deserialization error."

instance Serial1 Exp where
    serializeWith f t =
        case t of
            ELiteral l -> putWord8 0 >> serializeWith f l
            EAtom a -> putWord8 1 >> serializeWith f a
            EList l -> putWord8 2 >> serializeWith f l
            ESeparator s -> putWord8 3 >> serializeWith f s
    deserializeWith m =
        getWord8 >>= \a ->
            case a of
                0 -> deserializeWith m >>= return . ELiteral
                1 -> deserializeWith m >>= return . EAtom
                2 -> deserializeWith m >>= return . EList
                3 -> deserializeWith m >>= return . ESeparator
                _ -> fail "Exp: Deserialization error."

instance Serial1 LiteralExp where
    serializeWith f t =
        case t of
            LiteralExp {..} -> serialize _litLiteral >> f _litInfo
    deserializeWith m = do
        _litLiteral <- deserialize
        _litInfo <- m
        return $ LiteralExp {..}

instance Serial1 AtomExp where
    serializeWith f t =
        case t of
            AtomExp {..} -> do
                serialize _atomAtom
                mapM_ serialize _atomQualifiers
                f _atomInfo
    deserializeWith m = do
        _atomAtom <- deserialize
        _atomQualifiers <- deserialize
        _atomInfo <- m
        return $ AtomExp {..}

instance Serial1 ListExp where
    serializeWith f t =
        case t of
            ListExp {..} -> do
                serializeWith (serializeWith f) _listList
                serialize _listDelimiter
                f _listInfo
    deserializeWith m = do
        _listList <- deserializeWith $ deserializeWith m
        _listDelimiter <- deserialize
        _listInfo <- m
        return $ ListExp {..}

instance Serial1 SeparatorExp where
    serializeWith f t =
        case t of
            SeparatorExp {..} -> do
                serialize _sepSeparator
                f _sepInfo
    deserializeWith m = do
        _sepSeparator <- deserialize
        _sepInfo <- m
        return $ SeparatorExp {..}

instance Serial1 Def where
    serializeWith f Def {..} = do
        serialize _dDefName
        serialize _dModule
        serialize _dDefType
        serializeWith (serializeWith f) _dFunType
        serializeWith f _dDefBody
        serialize _dMeta
        serialize _dInfo
    deserializeWith m = do
        _dDefName <- deserialize
        _dModule <- deserialize
        _dDefType <- deserialize
        _dFunType <- deserializeWith (deserializeWith m)
        _dDefBody <- deserializeWith m
        _dMeta <- deserialize
        _dInfo <- deserialize
        return $ Def {..}

instance Serial1 Object where
  serializeWith f Object {..} = do
    pairListSerial1Helper serialize (serializeWith f) _oObject
    serializeWith (serializeWith f) _oObjectType
    serialize _oInfo
  deserializeWith m = do
    _oObject <- pairListDeSerial1Helper (const deserialize) deserializeWith m
    _oObjectType <- deserializeWith (deserializeWith m)
    _oInfo <- deserialize
    return $ Object {..}

deriving instance Serial NativeDefName

instance Serial NativeDFun where
  serialize (NativeDFun {..}) = serialize _nativeName
  deserialize = do
    _nativeName <- deserialize
    maybe
      (fail "Serial NativeDFun: deserialization error.")
      return
      (nativeDfunDeserialize _nativeName)

nativeDfunDeserialize :: NativeDefName -> Maybe NativeDFun
nativeDfunDeserialize nativename = Data.HashMap.Strict.lookup name nativeDefs >>= go
  where
    getText (NativeDefName text) = text
    name = Name (getText nativename) def
    go r =
        case r of
            Direct t ->
                case t of
                    TNative {..} -> return _tNativeFun
                    _ -> Nothing
            rr@(Ref _) -> go rr

instance Serial1 ConstVal where
    serializeWith f t =
        case t of
            CVRaw {..} -> do
                putWord8 0
                f _cvRaw
            CVEval {..} -> do
                putWord8 1
                f _cvRaw
                f _cvEval
    deserializeWith m =
        getWord8 >>= \a ->
            case a of
                0 -> do
                    _cvRaw <- m
                    return $ CVRaw {..}
                1 -> do
                    _cvRaw <- m
                    _cvEval <- m
                    return $ CVEval {..}
                _ -> fail "ConstVal: Deserialization error."

instance Serial1 App where
    serializeWith f t =
        case t of
            App {..} -> do
                f _appFun
                serializeWith f _appArgs
                serialize _appInfo
    deserializeWith m = do
        _appFun <- m
        _appArgs <- deserializeWith m
        _appInfo <- deserialize
        return $ App {..}

instance Serial1 BindType where
    serializeWith f t =
        case t of
            BindLet -> putWord8 0
            BindSchema {..} -> do
                putWord8 1
                f _bType
    deserializeWith m =
        getWord8 >>= \a ->
            case a of
                0 -> return BindLet
                1 -> do
                    _bType <- m
                    return $ BindSchema _bType
                _ -> fail "BindType: Deserialization error."


instance Serial1 Term where
    serializeWith f t =
        case t of
            TModule {..} -> do
                putWord8 0
                serializeWith (serializeWith f) _tModuleDef
                serializeWith f _tModuleBody
                serialize _tInfo
            TList {..} -> do
                putWord8 1
                serializeWith (serializeWith f) _tList
                serializeWith (serializeWith f) _tListType
                serialize _tInfo
            TDef {..} -> do
                putWord8 2
                serializeWith f _tDef
                serialize _tInfo
            TNative {..} -> do
                putWord8 3
                serialize _tNativeName
                serialize _tNativeFun
                serializeWith (serializeWith (serializeWith f)) _tFunTypes
                serialize _tNativeDocs
                serialize _tNativeTopLevelOnly
                serialize _tInfo
            TConst {..} -> do
                putWord8 4
                serializeWith (serializeWith f) _tConstArg
                serialize _tModule
                serializeWith (serializeWith f) _tConstVal
                serialize _tMeta
                serialize _tInfo
            TApp {..} -> do
                putWord8 5
                serializeWith (serializeWith f) _tApp
                serialize _tInfo
            TVar {..} -> do
                putWord8 6
                f _tVar
                serialize _tInfo
            TBinding {..} -> do
                putWord8 7
                pairListSerial1Helper
                    (serializeWith (serializeWith f))
                    (serializeWith f)
                    _tBindPairs
                serializeWith f _tBindBody
                serializeWith (serializeWith (serializeWith f)) _tBindType
                serialize _tInfo
            TObject {..} -> do
                putWord8 8
                serializeWith f _tObject
                serialize _tInfo
            TSchema {..} -> do
                putWord8 9
                serialize _tSchemaName
                serialize _tModule
                serialize _tMeta
                serializeWith (serializeWith (serializeWith f)) _tFields
                serialize _tInfo
            TLiteral {..} -> do
                putWord8 10
                serialize _tLiteral
                serialize _tInfo
            TGuard {..} -> do
                putWord8 11
                serialize _tGuard
                serialize _tInfo
            TUse {..} -> do
                putWord8 12
                serialize _tUse
                serialize _tInfo
            TValue {..} -> do
                putWord8 13
                serialize _tValue
                serialize _tInfo
            TStep {..} -> do
                putWord8 14
                serializeWith (serializeWith f) _tStepEntity
                serializeWith f _tStepExec
                serializeWith (serializeWith f) _tStepRollback
                serialize _tInfo
            TTable {..} -> do
                putWord8 15
                serialize _tTableName
                serialize _tModule
                serialize _tHash
                serializeWith (serializeWith f) _tTableType
                serialize _tMeta
                serialize _tInfo
    deserializeWith m =
        getWord8 >>= \a ->
            case a of
                0 -> do
                    _tModuleDef <- deserializeWith (deserializeWith m)
                    _tModuleBody <- deserializeWith m
                    _tInfo <- deserialize
                    return $ TModule {..}
                1 -> do
                    _tList <- deserializeWith (deserializeWith m)
                    _tListType <- deserializeWith (deserializeWith m)
                    _tInfo <- deserialize
                    return $ TList {..}
                2 -> do
                    _tDef <- deserializeWith m
                    _tInfo <- deserialize
                    return $ TDef {..}
                3 -> do
                    _tNativeName <- deserialize
                    _tNativeFun <- deserialize
                    _tFunTypes <- deserializeWith (deserializeWith (deserializeWith m))
                    _tNativeExamples <- deserialize
                    _tNativeDocs <- deserialize
                    _tNativeTopLevelOnly <- deserialize
                    _tInfo <- deserialize
                    return $ TNative {..}
                4 -> do
                    _tConstArg <- deserializeWith (deserializeWith m)
                    _tModule <- deserialize
                    _tConstVal <- deserializeWith (deserializeWith m)
                    _tMeta <- deserialize
                    _tInfo <- deserialize
                    return $ TConst {..}
                5 -> do
                    _tApp <- deserializeWith (deserializeWith m)
                    _tInfo <- deserialize
                    return $ TApp {..}
                6 -> do
                    _tVar <- m
                    _tInfo <- deserialize
                    return $ TVar {..}
                7 -> do
                    _tBindPairs <-
                        pairListDeSerial1Helper
                            (deserializeWith . deserializeWith)
                            deserializeWith
                            m
                    _tBindBody <- deserializeWith m
                    _tBindType <- deserializeWith (deserializeWith (deserializeWith m))
                    _tInfo <- deserialize
                    return $ TBinding {..}
                8 -> do
                    _tObject <- deserializeWith m
                    _tInfo <- deserialize
                    return $ TObject {..}
                9 -> do
                    _tSchemaName <- deserialize
                    _tModule <- deserialize
                    _tMeta <- deserialize
                    _tFields <- deserializeWith (deserializeWith (deserializeWith m))
                    _tInfo <- deserialize
                    return $ TSchema {..}
                10 -> do
                    _tLiteral <- deserialize
                    _tInfo <- deserialize
                    return $ TLiteral {..}
                11 -> do
                    _tGuard <- deserialize
                    _tInfo <- deserialize
                    return $ TGuard {..}
                12 -> do
                    _tUse <- deserialize
                    _tInfo <- deserialize
                    return $ TUse {..}
                13 -> do
                    _tValue <- deserialize
                    _tInfo <- deserialize
                    return $ TValue {..}
                14 -> do
                    _tStepEntity <- deserializeWith (deserializeWith m)
                    _tStepExec <- deserializeWith m
                    _tStepRollback <- deserializeWith (deserializeWith m)
                    _tInfo <- deserialize
                    return $ TStep {..}
                15 -> do
                    _tTableName <- deserialize
                    _tModule <- deserialize
                    _tHash <- deserialize
                    _tTableType <- deserializeWith (deserializeWith m)
                    _tMeta <- deserialize
                    _tInfo <- deserialize
                    return $ TTable {..}
                _ -> fail "Term: Deserialization error."

pairListSerial1Helper ::
     (MonadPut m, Foldable t1)
  => (t2 -> m a)
  -> (t3 -> m b)
  -> t1 (t2, t3)
  -> m ()
pairListSerial1Helper f g xs = forM_ xs (uncurry go) >> putWord8 1
  where
    go a b = putWord8 0 >> f a >> g b

pairListDeSerial1Helper ::
     MonadGet m => (t -> m a1) -> (t -> m a2) -> t -> m [(a1, a2)]
pairListDeSerial1Helper f g m = go id
  where
    go dl = do
        a <- getWord8
        if a == 1
           then return $ dl []
           else do
            p <- (,) <$> f m <*> g m
            go (dl . (p :))

deriving instance Serial Guard

deriving instance Serial ModuleGuard

deriving instance Serial UserGuard

deriving instance Serial KeySet

deriving instance Serial KeySetName

deriving instance Serial Name

deriving instance Serial PublicKey

deriving instance Serial PactGuard

deriving instance Serial PactId

deriving instance Serial TypeName

deriving instance Serial SchemaPartial

deriving instance Serial Use

deriving instance Serial Hash

deriving instance Serial Value

instance Serial a => Serial (Vector.Vector a) where
    serialize = mapM_ serialize . Vector.toList
    deserialize = deserialize >>= return . Vector.fromList

deriving instance Serial TableName

deriving instance (Generic n, Serial n) => Serial (Arg (Term n))

deriving instance (Generic n, Serial n) => Serial (Type (Term n))

deriving instance
         (Generic n, Serial n) => Serial (BindType (Type (Term n)))

deriving instance (Generic n, Serial n) => Serial (Term n)

deriving instance
         (Generic n, Serial n) => Serial (FunType (Term n))

deriving instance
         (Generic n, Serial n) => Serial (ConstVal (Term n))

deriving instance (Generic n, Serial n) => Serial (App (Term n))

deriving instance
         (Generic n, Serial n) => Serial (TypeVar (Term n))

deriving instance (Generic n, Serial n) => Serial (Def n)

deriving instance (Generic g, Serial g) => Serial (Governance g)

deriving instance (Generic g, Serial g) => Serial (Module g)

deriving instance Serial PrimType

deriving instance Serial GuardType

deriving instance Serial SchemaType

instance Serial1 FunType where
    serializeWith f (FunType {..}) = do
        serializeWith (serializeWith f) _ftArgs
        serializeWith f _ftReturn
    deserializeWith m = do
        _ftArgs <- deserializeWith (deserializeWith m)
        _ftReturn <- deserializeWith m
        return $ FunType {..}

instance Serial1 Arg where
    serializeWith f (Arg {..}) = serialize _aName >> serializeWith f _aType >> serialize _aInfo
    deserializeWith m = do
        _aName <- deserialize
        _aType <- deserializeWith m
        _aInfo <- deserialize
        return $ Arg {..}

instance Serial1 Type where
    serializeWith f t =
        case t of
            TyAny -> putWord8 0
            TyVar {..} -> putWord8 1 >> serializeWith f _tyVar
            TyPrim p -> putWord8 2 >> serialize p
            TyList {..} -> putWord8 3 >> serializeWith f _tyListType
            TySchema {..} -> do
                putWord8 4
                serialize _tySchema
                serializeWith f _tySchemaType
            TyFun {..} -> putWord8 5 >> serializeWith f _tyFunType
            TyUser {..} -> putWord8 6 >> f _tyUser
    deserializeWith m =
        getWord8 >>= \a ->
            case a of
                0 -> return TyAny
                1 -> TyVar <$> deserializeWith m
                2 -> TyPrim <$> deserialize
                3 -> TyList <$> deserializeWith m
                4 -> TySchema <$> deserialize <*> (deserializeWith m) <*> deserialize
                5 -> TyFun <$> deserializeWith m
                6 -> TyUser <$> m
                _ -> fail "Type: Deserialization error."

deriving instance Serial TypeVarName

instance Serial1 TypeVar where
    serializeWith f v =
        case v of
            TypeVar {..} -> do
                putWord8 0
                serialize _tvName
                serializeWith (serializeWith f) _tvConstraint
            SchemaVar {..} -> putWord8 1 >> serialize _tvName
    deserializeWith m =
        getWord8 >>= \a ->
            case a of
                0 -> do
                    _tvName <- deserialize
                    _tvConstraint <- deserializeWith (deserializeWith m)
                    return $ TypeVar {..}
                1 -> liftM SchemaVar deserialize
                _ -> error "TypeVar: Deserialization error."

-------------------------
-- SERIALIZE INSTANCES --
-------------------------

deriving instance Serialize TableId

instance Serialize (Table DataKey) where
    put (DataTable t) = put t
    get = DataTable <$> get

instance Serialize (Table TxKey) where
    put (TxTable t) = put t
    get = TxTable <$> get

deriving instance Serialize (TxLog Value)

deriving instance Serialize TxId

deriving instance Serialize SQLiteConfig

deriving instance Serialize Pragma

deriving instance Serialize CommandState

deriving instance Serialize Name

deriving instance Serialize ModuleName

deriving instance Serialize NamespaceName

deriving instance Serialize Info

deriving instance Serialize Code

deriving instance Serialize Parsed

deriving instance Serialize Delta

deriving instance (Serialize a, Generic a) => Serialize (ModuleDef a)

deriving instance Serialize ModuleData

deriving instance Serialize Ref

deriving instance (Generic g, Serialize g) => Serialize (Governance g)

deriving instance (Generic g, Serialize g) => Serialize (Module g)

deriving instance Serialize Interface

deriving instance Serialize Use

deriving instance Serialize Meta

deriving instance Serialize (Exp Info)

deriving instance Serialize (LiteralExp Info)

deriving instance Serialize (AtomExp Info)

deriving instance Serialize (ListExp Info)

deriving instance Serialize ListDelimiter

deriving instance Serialize (SeparatorExp Info)

deriving instance Serialize Separator

deriving instance Serialize KeySetName

instance Serialize RefStore where
    put RefStore {..} = put _rsModules
    get = do
        let _rsNatives = nativeDefs
        _rsModules <- get
        return $ RefStore {..}

deriving instance Serialize SchemaPartial
deriving instance
         (Generic n, Serialize n) => Serialize (Type (Term n))

deriving instance Serialize SchemaType

deriving instance Serialize PrimType

deriving instance Serialize GuardType

deriving instance
         (Generic n, Serialize n) => Serialize (FunType (Term n))

deriving instance
         (Generic n, Serialize n) => Serialize (Arg (Term n))

deriving instance
         (Generic n, Serialize n) => Serialize (TypeVar (Term n))

deriving instance Serialize TypeVarName

deriving instance Serialize DefName

deriving instance Serialize DefType

instance (Generic n, Serialize n) => Serialize (Def n) where
    put Def {..} = do
        put _dDefName
        put _dModule
        put _dDefType
        put _dFunType
        put _dDefBody
        put _dMeta
        put _dInfo
    get = Def <$> get <*> get <*> get <*> get <*> get <*> get <*> get

deriving instance Serialize NativeDefName

deriving instance
         (Generic n, Serialize n) => Serialize (FunTypes (Term n))

deriving instance
         (Generic n, Serialize n) => Serialize (ConstVal (Term n))

deriving instance
         (Generic n, Serialize n) => Serialize (App (Term n))

deriving instance
         (Generic n, Serialize n) => Serialize (BindType (Type (Term n)))

deriving instance Serialize TypeName

deriving instance Serialize Guard

deriving instance Serialize UserGuard

deriving instance Serialize ModuleGuard

deriving instance Serialize KeySet

deriving instance Serialize PactGuard

deriving instance Serialize PactId

deriving instance Serialize TableName

deriving instance Serialize Example

instance (Generic n, Serialize n) => Serialize (Term n) where
    put t =
        case t of
            TModule {..} -> do
                putWord8 0
                put _tModuleDef
                put _tModuleBody
                put _tInfo
            TList {..} -> do
                putWord8 1
                put _tList
                put _tListType
                put _tInfo
            TDef {..} -> do
                putWord8 2
                put _tDef
                put _tInfo
            TNative {..} -> do
                putWord8 3
                put _tNativeName
                put _tNativeFun
                put _tFunTypes
                put _tFunTypes
            TConst {..} -> do
                putWord8 4
                put _tConstArg
                put _tModule
                put _tConstVal
                put _tMeta
                put _tInfo
            TApp {..} -> do
                putWord8 5
                put _tApp
                put _tInfo
            TVar {..} -> do
                putWord8 6
                put _tVar
                put _tInfo
            TBinding {..} -> do
                putWord8 7
                put _tBindPairs
                put _tBindBody
                put _tBindType
                put _tInfo
            TObject {..} -> do
                putWord8 8
                put _tObject
                put _tInfo
            TSchema {..} -> do
                putWord8 9
                put _tSchemaName
                put _tModule
                put _tMeta
                put _tFields
                put _tInfo
            TLiteral {..} -> do
                putWord8 10
                put _tLiteral
                put _tInfo
            TGuard {..} -> do
                putWord8 11
                put _tGuard
                put _tInfo
            TUse {..} -> do
                putWord8 12
                put _tUse
                put _tInfo
            TValue {..} -> do
                putWord8 13
                put _tValue
                put _tInfo
            TStep {..} -> do
                putWord8 14
                put _tStepEntity
                put _tStepExec
                put _tStepRollback
                put _tInfo
            TTable {..} -> do
                putWord8 15
                put _tTableName
                put _tModule
                put _tHash
                put _tTableType
                put _tMeta
                put _tInfo
    get =
        getWord8 >>= \a ->
            case a of
                0 -> do
                    _tModuleDef <- get
                    _tModuleBody <- get
                    _tInfo <- get
                    return $ TModule {..}
                1 -> do
                    _tList <- get
                    _tListType <- get
                    _tInfo <- get
                    return $ TList {..}
                2 -> do
                    _tDef <- get
                    _tInfo <- get
                    return $ TDef {..}
                3 -> do
                    _tNativeName <- get
                    _tNativeFun <- get
                    _tFunTypes <- get
                    _tNativeExamples <- get
                    _tNativeDocs <- get
                    _tNativeTopLevelOnly <- get
                    _tInfo <- get
                    return $ TNative {..}
                4 -> do
                    _tConstArg <- get
                    _tModule <- get
                    _tConstVal <- get
                    _tMeta <- get
                    _tInfo <- get
                    return $ TConst {..}
                5 -> do
                    _tApp <- get
                    _tInfo <- get
                    return $ TApp {..}
                6 -> do
                    _tVar <- get
                    _tInfo <- get
                    return $ TVar {..}
                7 -> do
                    _tBindPairs <- get
                    _tBindBody <- get
                    _tBindType <- get
                    _tInfo <- get
                    return $ TBinding {..}
                8 -> do
                    _tObject <- get
                    _tInfo <- get
                    return $ TObject {..}
                9 -> do
                    _tSchemaName <- get
                    _tModule <- get
                    _tMeta <- get
                    _tFields <- get
                    _tInfo <- get
                    return $ TSchema {..}
                10 -> do
                    _tLiteral <- get
                    _tInfo <- get
                    return $ TLiteral {..}
                11 -> do
                    _tGuard <- get
                    _tInfo <- get
                    return $ TGuard {..}
                12 -> do
                    _tUse <- get
                    _tInfo <- get
                    return $ TUse {..}
                13 -> do
                    _tValue <- get
                    _tInfo <- get
                    return $ TValue {..}
                14 -> do
                    _tStepEntity <- get
                    _tStepExec <- get
                    _tStepRollback <- get
                    _tInfo <- get
                    return $ TStep {..}
                15 -> do
                    _tTableName <- get
                    _tModule <- get
                    _tHash <- get
                    _tTableType <- get
                    _tMeta <- get
                    _tInfo <- get
                    return $ TTable {..}
                _ -> fail "Term: get error."

instance Serialize NativeDFun where
    put NativeDFun {..} = put _nativeName
    get = do
        _nativeName <- get
        maybe
          (fail "Serial NativeDFun: deserialization error.")
          return
          (nativeDfunDeserialize _nativeName)

instance (Eq h, Hashable h, Serialize h) => Serialize (HashSet h) where
    put = put . Data.HashSet.toList
    get = get >>= return . Data.HashSet.fromList

instance (Hashable k, Ord k, Serialize k, Serialize v) => Serialize (HashMap k v) where
    put = put . Data.HashMap.Strict.toList
    get = get >>= return . Data.HashMap.Strict.fromList

deriving instance (Generic n, Serialize n) => Serialize (Object n)

deriving instance Serialize FieldKey

deriving instance Serialize PactExec

deriving instance Serialize PactContinuation
