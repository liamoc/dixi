{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Dixi.Page where

import Control.Lens
import Data.Data
import Data.Monoid
import Data.SafeCopy
import Data.Text

import Dixi.Database.Orphans ()

data Page b = Page { _body :: b, _comment :: Last Text }
              deriving (Functor, Data, Typeable, Show)

deriveSafeCopy 0 'base ''Page

makeLenses ''Page

instance Monoid b => Monoid (Page b) where
  mempty = Page mempty mempty
  mappend (Page a a') (Page b b') = Page (a <> b) (a' <> b')
