{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP                #-}
module Dixi.Page where

import Control.Lens
import Data.Data
import Data.Monoid
import Data.SafeCopy
import Data.Text
import Data.Time
#ifdef OLDBASE
import Data.Foldable
#endif

import Dixi.Database.Orphans ()

data Page b = Page { _body :: b, _comment :: Last Text, _time :: Last UTCTime }
              deriving (Functor, Data, Typeable, Show, Foldable, Traversable)

deriveSafeCopy 0 'base ''Page

makeLenses ''Page

instance Monoid b => Monoid (Page b) where
  mempty = Page mempty mempty mempty
  mappend (Page a a' a'') (Page b b' b'') = Page (a <> b) (a' <> b') (a'' <> b'')
