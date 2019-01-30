{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module: Chainweb.Pact.Backend.Orphans
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb

module Chainweb.Pact.Backend.Orphans where

{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Data.HashMap.Strict as HMS

import Data.Bytes.Serial
import Data.Serialize

import GHC.Generics

import Text.Trifecta.Delta

import qualified Pact.Persist as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Persistence as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- deriving instance Generic P.TableId
-- deriving instance Serialize P.TableId

-- instance Serialize (P.Table k) where
--   put (P.DataTable t) = put t >> put False
--   put (P.TxTable t) = put t >> put True
--   get = undefined
--         {-do
--     v <- get :: Get P.TableId
--     b <- get :: Get Bool
--     if b
--       then return $ P.DataTable v
--       else return $ P.TxTable v-}

-- deriving instance Serialize v => Serialize (P.TxLog v)
-- deriving instance Serialize P.TxId
-- deriving instance Serialize P.SQLiteConfig
-- deriving instance Generic P.Pragma
-- deriving instance Serialize P.Pragma

-- instance Generic (HMS.HashMap k v) where
--   from = undefined
--   to = undefined

-- instance Serialize (HMS.HashMap k v) where
--   put = undefined
--   get = undefined

-- deriving instance Generic P.CommandPact
-- deriving instance Serialize P.CommandPact

-- deriving instance Generic P.RefStore

-- instance Serialize P.RefStore where
--   put (P.RefStore {..}) = put _rsModules
--   get = get

-- deriving instance Generic P.CommandState
-- deriving instance Serialize P.CommandState

-- deriving instance Serialize P.Name
-- deriving instance Serialize P.ModuleName
-- deriving instance Serialize P.NamespaceName
-- deriving instance Serialize P.Info
-- deriving instance Serialize P.Parsed
-- deriving instance Serialize Delta
-- deriving instance Serialize P.Code
-- deriving instance Generic (P.Term n)
-- deriving instance Serialize (P.Term n)

-- deriving instance Serialize t => Serialize (P.Type t)
-- deriving instance Serialize t => Serialize (P.TypeVar t)
-- deriving instance Serialize P.TypeVarName
-- deriving instance Serialize t => Serialize (P.FunType t)
-- deriving instance Serialize t => Serialize (P.Arg t)
-- deriving instance Serialize P.PrimType
-- deriving instance Serialize P.GuardType
-- deriving instance Serialize P.SchemaType
-- deriving instance Generic n => Generic (P.Def n)
-- deriving instance (Generic n, Serialize n) => Serialize (P.Def n)
-- deriving instance Generic P.DefName
-- deriving instance Serialize P.DefName
-- deriving instance Serialize P.DefType
-- deriving instance Serialize i => Serialize (P.Exp i)
-- deriving instance Serialize i => Serialize (P.LiteralExp i)
-- deriving instance Serialize i => Serialize (P.AtomExp i)
-- deriving instance Serialize i => Serialize (P.ListExp i)
-- deriving instance Serialize P.ListDelimiter
-- deriving instance Serialize i => Serialize (P.SeparatorExp i)
-- deriving instance Serialize P.Separator
-- deriving instance Serialize P.Meta
