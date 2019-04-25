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
import Data.HashMap.Strict
import Data.Thyme.Clock
import Data.Thyme.Internal.Micro
import qualified Data.Vector as Vector

import GHC.Generics hiding (Meta)

import Text.Trifecta.Delta

import Pact.Native
import Pact.Persist
import Pact.Persist.SQLite
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.Runtime
import Pact.Types.Server

-----------------------
-- GENERIC INSTANCES --
-----------------------

deriving instance Generic (Governance g)
deriving instance Generic CommandState
deriving instance Generic Decimal
deriving instance Generic DefName
deriving instance Generic FieldKey
deriving instance Generic KeySetName
deriving instance Generic ModuleData
deriving instance Generic NativeDefName
deriving instance Generic PactContinuation
deriving instance Generic PactExec
deriving instance Generic PactValue
deriving instance Generic Pragma
deriving instance Generic Ref
deriving instance Generic RefStore
deriving instance Generic TableId
deriving instance Generic TableName

----------------------
-- SERIAL INSTANCES --
----------------------

instance Serial RefStore where
    serialize RefStore {..} = serialize _rsModules
    deserialize = do
        let _rsNatives = nativeDefs
        _rsModules <- deserialize
        return $ RefStore {..}

deriving instance (Generic a, Serial a) => Serial (App (Term a))
deriving instance (Generic a, Serial a) => Serial (Arg a)
deriving instance (Generic a, Serial a) => Serial (BindPair a)
deriving instance (Generic a, Serial a) => Serial (BindType (Type (Term a)))
deriving instance (Generic a, Serial a) => Serial (ConstVal (Term a))
deriving instance (Generic a, Serial a) => Serial (Def a)
deriving instance (Generic a, Serial a) => Serial (FunType a)
deriving instance (Generic a, Serial a) => Serial (Governance a)
deriving instance (Generic a, Serial a) => Serial (Module a)
deriving instance (Generic a, Serial a) => Serial (ModuleDef a)
deriving instance (Generic a, Serial a) => Serial (Object a)
deriving instance (Generic a, Serial a) => Serial (ObjectMap a)
deriving instance (Generic a, Serial a) => Serial (Term a)
deriving instance (Generic a, Serial a) => Serial (Type a)
deriving instance (Generic a, Serial a) => Serial (TypeVar a)
deriving instance Serial (AtomExp Info)
deriving instance Serial (Exp Info)
deriving instance Serial (ListExp Info)
deriving instance Serial (LiteralExp Info)
deriving instance Serial (SeparatorExp Info)
deriving instance Serial (TxLog Value)
deriving instance Serial Code
deriving instance Serial CommandState
deriving instance Serial Decimal
deriving instance Serial DefName
deriving instance Serial DefType
deriving instance Serial Delta
deriving instance Serial Example
deriving instance Serial FieldKey
deriving instance Serial Guard
deriving instance Serial GuardType
deriving instance Serial Hash
deriving instance Serial Info
deriving instance Serial Interface
deriving instance Serial KeySet
deriving instance Serial KeySetName
deriving instance Serial ListDelimiter
deriving instance Serial Literal
deriving instance Serial Meta
deriving instance Serial Micro
deriving instance Serial ModuleData
deriving instance Serial ModuleGuard
deriving instance Serial ModuleName
deriving instance Serial Name
deriving instance Serial NamespaceName
deriving instance Serial NominalDiffTime
deriving instance Serial PactContinuation
deriving instance Serial PactExec
deriving instance Serial PactGuard
deriving instance Serial PactId
deriving instance Serial PactValue
deriving instance Serial Parsed
deriving instance Serial Pragma
deriving instance Serial PrimType
deriving instance Serial PublicKey
deriving instance Serial Ref
deriving instance Serial SQLiteConfig
deriving instance Serial SchemaPartial
deriving instance Serial SchemaType
deriving instance Serial Separator
deriving instance Serial TableId
deriving instance Serial TableName
deriving instance Serial TxId
deriving instance Serial TypeName
deriving instance Serial UTCTime
deriving instance Serial Use
deriving instance Serial UserGuard
deriving instance Serial Value

instance Serial1 Governance where
  serializeWith f (Governance t) = case t of
    Left r -> putWord8 0 >> serialize r
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
            -- TValue {..} -> do
            --     putWord8 13
            --     serialize _tValue
            --     serialize _tInfo
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
                -- 13 -> do
                --     _tValue <- deserialize
                --     _tInfo <- deserialize
                --     return $ TValue {..}
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

instance Serial a => Serial (Vector.Vector a) where
    serialize = mapM_ serialize . Vector.toList
    deserialize = deserialize >>= return . Vector.fromList

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

instance Serial (Table DataKey) where
  serialize (DataTable t) = serialize t
  deserialize = DataTable <$> deserialize

instance Serial (Table TxKey) where
  serialize (TxTable t) = serialize t
  deserialize = TxTable <$> deserialize
