module Dixi.Hamlet where

import Text.Hamlet
import Language.Haskell.TH.Quote

hml :: QuasiQuoter
hml = hamletWithSettings htmlRules (defaultHamletSettings { hamletNewlines = NoNewlines})
