{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module: Chainweb.Pact.Backend.Orphans
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb

module Chainweb.Pact.Backend.Orphans where

{-# OPTIONS_GHC -fno-warn-orphans #-}


import Control.Monad

import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bytes.Serial
import Data.HashMap.Strict
import Data.HashSet
import Data.Hashable
import Data.List.NonEmpty
import Data.Serialize hiding (putWord8, getWord8)
import Data.Word

import GHC.Generics hiding (Meta)

import Text.Trifecta.Delta

import Pact.Persist
import Pact.Persist.SQLite
import Pact.Types.Persistence
import Pact.Types.Runtime
import Pact.Types.Server

deriving instance Generic TableId
deriving instance Serialize TableId

instance Serialize (Table k) where
  put (DataTable t) = put t >> put False
  put (TxTable t) = put t >> put True
  get = undefined               -- TODO: How do I solve this?
        {-do
    v <- get :: Get TableId
    b <- get :: Get Bool
    if b
      then return $ DataTable v
      else return $ TxTable v-}

deriving instance Serialize v => Serialize (TxLog v)
deriving instance Serialize TxId
deriving instance Serialize SQLiteConfig
deriving instance Generic Pragma
deriving instance Serialize Pragma

instance (Hashable k, Ord k, Serialize k, Serialize v) => Serialize (HashMap k v) where
    put = put . Data.HashMap.Strict.toList
    get = get >>= return . Data.HashMap.Strict.fromList

deriving instance Generic CommandPact
deriving instance Serialize CommandPact

deriving instance Generic RefStore

deriving instance Generic ModuleData
deriving instance Serialize ModuleData

deriving instance Generic Ref
deriving instance Serialize Ref

instance Serialize RefStore where
    put (RefStore {..}) = put _rsModules
    get = do
      let natives = undefined   -- TODO: Where do I get this from?
      modules <- get
      return $ RefStore natives modules

deriving instance Generic CommandState
deriving instance Serialize CommandState

deriving instance Serialize Name
deriving instance Serialize ModuleName
deriving instance Serialize NamespaceName
deriving instance Serialize Info

instance Serial Info where
    serialize = undefined
    deserialize = undefined

deriving instance Serialize Parsed
deriving instance Serialize Delta
deriving instance Serialize Code



instance Serial1 Def where
  serializeWith f t = undefined
  deserializeWith m = undefined

instance Serial1 Term where
  serializeWith f t =
    case t of
      TModule {..} -> do
        putWord8 0
        serialize _tModuleDef
        serializeWith f _tModuleBody
        serialize _tInfo
      TList {..} -> do
         putWord8 1
         mapM_ (serializeWith f) _tList
         serializeWith (serializeWith f) _tListType
      TDef {..} -> do
         putWord8 2
         serializeWith f _tDef
         serialize _tInfo
      TNative {..} -> putWord8 3 >> undefined
      TConst {..} -> putWord8 4 >> undefined
      TApp {..} -> putWord8 5 >> undefined
      TVar {..} -> putWord8 6 >> undefined
      TBinding {..} -> putWord8 7 >> undefined
      TObject {..} -> putWord8 8 >> undefined
      TSchema {..} -> putWord8 9 >> undefined
      TLiteral {..} -> putWord8 10 >> undefined
      TGuard {..} -> putWord8 11 >> undefined
      TUse {..} -> putWord8 12 >> undefined
      TValue {..} -> putWord8 13 >> undefined
      TStep {..} -> putWord8 14 >> undefined
      TTable {..} -> putWord8 15 >> undefined
  deserializeWith = undefined

instance Serialize n => Serialize (Term n) where
  put = undefined
  get = undefined

instance Serialize NativeDFun where
  put = undefined
  get = undefined


deriving instance Serialize TypeName
deriving instance Generic Guard
deriving instance Serialize Guard
deriving instance Serialize PactGuard
deriving instance Serialize KeySet
deriving instance Generic PactId
deriving instance Serialize PactId
deriving instance Generic TableName
deriving instance Serialize TableName
deriving instance Serialize ModuleGuard
deriving instance Serialize UserGuard

deriving instance Generic NativeDefName
deriving instance Serialize NativeDefName

deriving instance (Generic n, Serialize n) => Serialize (NonEmpty (FunType (Term n)))
deriving instance (Generic n, Serialize n) => Serialize (ConstVal (Term n))
deriving instance (Generic n) => Generic (App (Term n))
deriving instance (Generic n, Serialize n) => Serialize (App (Term n))
deriving instance Generic (BindType (Type (Term n)))
deriving instance (Generic n, Serialize n) => Serialize (BindType (Type (Term n)))

deriving instance Generic Module
deriving instance Serialize Module
instance Serial Module where
  serialize = undefined
  deserialize = undefined

deriving instance Generic KeySetName
deriving instance Serialize KeySetName

deriving instance Generic Use
deriving instance Serialize Use

-- Let's just do the simple thing right now.
instance (Eq h, Hashable h, Serialize h) => Serialize (HashSet h) where
  put = put . Data.HashSet.toList
  get = get >>= return . Data.HashSet.fromList

deriving instance Serial PrimType
deriving instance Serial GuardType
deriving instance Serial SchemaType

instance Serial1 FunType where
  serializeWith f (FunType {..}) =
    mapM_ (serializeWith f) _ftArgs >> serializeWith f _ftReturn
  deserializeWith m = do
    _ftArgs <- deserializeWith (deserializeWith m)
    _ftReturn <- deserializeWith m
    return $ FunType {..}

instance Serial1 Arg where
  serializeWith f (Arg {..}) =
    serialize _aName >> serializeWith f _aType >> serialize _aInfo
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
      TyUser {..} -> putWord8 6 >> undefined _tyUser -- TODO: How?
  deserializeWith m =
    getWord8 >>= \a ->
      case a of
        0 -> return TyAny
        1 -> liftM TyVar $ deserializeWith m
        2 -> liftM TyPrim $ deserialize
        3 -> liftM TyList $ deserializeWith m
        4 -> liftM2 TySchema deserialize (deserializeWith m)
        5 -> liftM TyFun $ deserializeWith m
        6 -> liftM TyUser undefined -- TODO: Again how?

deriving instance Serial TypeVarName

instance Serial1 TypeVar where
  serializeWith f v =
    case v of
      TypeVar {..} -> do
        putWord8 0
        serialize _tvName
        mapM_ (serializeWith f) _tvConstraint
      SchemaVar {..} -> putWord8 1 >> serialize _tvName
  deserializeWith m =
    getWord8 >>= \a ->
      case a of
        0 -> do
          _tvName <- deserialize
          _tvConstraint <- deserializeWith (deserializeWith m)
          return $ TypeVar {..}
        1 -> liftM SchemaVar deserialize
        _ -> error ""

deriving instance Serialize t => Serialize (Type t)
deriving instance Serialize t => Serialize (TypeVar t)
deriving instance Serialize TypeVarName
deriving instance Serialize t => Serialize (FunType t)
deriving instance Serialize t => Serialize (Arg t)
deriving instance Serialize PrimType
deriving instance Serialize GuardType
deriving instance Serialize SchemaType

instance (Serialize n) => Serialize (Def n) where
  put (Def {..}) =
    put _dDefName >> put _dModule >> put _dDefType >> put _dFunType >>
    put _dDefBody >>
    put _dMeta >>
    put _dInfo
  get = undefined

deriving instance Generic DefName
deriving instance Serialize DefName
deriving instance Serialize DefType
deriving instance Serialize i => Serialize (Exp i)
deriving instance Serialize i => Serialize (LiteralExp i)
deriving instance Serialize i => Serialize (AtomExp i)
deriving instance Serialize i => Serialize (ListExp i)
deriving instance Serialize ListDelimiter
deriving instance Serialize i => Serialize (SeparatorExp i)
deriving instance Serialize Separator
deriving instance Serialize Meta
