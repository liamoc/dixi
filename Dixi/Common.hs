{-# LANGUAGE DeriveDataTypeable #-}
module Dixi.Common where

import Data.Text
import Data.Typeable

type Key = Text
type Version = Int

data DixiError = VersionNotFound Key Version
               | PatchNotApplicable Key
               deriving (Show, Typeable)
