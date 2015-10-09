{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# OPTIONS -fno-warn-orphans   #-}
module Dixi.Database.Orphans where

import Control.Lens ()
import Data.Compositions.Internal      as C
import Data.Compositions.Snoc.Internal as S
import Data.Data
import Data.Foldable
import Data.Monoid
import Data.Patch.Internal
import Data.SafeCopy
import Data.Traversable

deriveSafeCopy 0 'base ''Node
deriveSafeCopy 0 'base ''C.Compositions
deriveSafeCopy 0 'base ''Flip
deriveSafeCopy 0 'base ''S.Compositions
deriveSafeCopy 0 'base ''Edit
deriveSafeCopy 0 'base ''Patch
deriveSafeCopy 0 'base ''Last
deriveSafeCopy 0 'base ''HunkStatus

deriving instance Foldable    (Last)
deriving instance Traversable (Last)

deriving instance Typeable (Patch)
deriving instance Typeable (HunkStatus)

deriving instance Data a => (Data (Last a))

