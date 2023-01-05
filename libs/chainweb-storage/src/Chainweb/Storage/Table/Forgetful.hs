{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module: Chainweb.Storage.Table.Forgetful
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A key-value store that forgets everything that is stored in it.
--
module Chainweb.Storage.Table.Forgetful
( ForgetfulTable(..)
) where

import Chainweb.Storage.Table

-- | A key-value store that forgets everything that is added to it.
--
data ForgetfulTable k v = ForgetfulTable

instance ReadableTable (ForgetfulTable k v) k v where
    tableLookup _ _ = return Nothing
    tableMember _ _ = return False
    {-# INLINE tableLookup #-}

instance Table (ForgetfulTable k v) k v where
    tableInsert _ _ _ = return ()
    tableDelete _ _ = return ()
    {-# INLINE tableInsert #-}
    {-# INLINE tableDelete #-}

