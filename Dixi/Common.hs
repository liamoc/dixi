module Dixi.Common where

import Data.Text

type Key = Text
type Version = Int

data DixiError = VersionNotFound Key Version
               | PatchNotApplicable Key
