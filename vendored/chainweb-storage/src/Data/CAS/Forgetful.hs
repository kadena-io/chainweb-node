{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Data.CAS.Forgetful
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A content addressable store that forgets everything that is stored in it.
--
module Data.CAS.Forgetful
( ForgetfulCas(..)
) where

import Data.CAS

-- | A content addressable store that forgets everything that is added to it.
--
data ForgetfulCas a = ForgetfulCas

instance IsCasValue a => HasCasLookup (ForgetfulCas a) where
    type CasValueType (ForgetfulCas a) = a
    casLookup _ _ = return Nothing
    {-# INLINE casLookup #-}

instance IsCasValue a => IsCas (ForgetfulCas a) where
    casInsert _ _ = return ()
    casDelete _ _ = return ()
    {-# INLINE casInsert #-}
    {-# INLINE casDelete #-}

