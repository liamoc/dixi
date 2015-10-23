module Dixi.PatchUtils where

import Data.Foldable
import Data.Monoid
import Data.Patch  (Patch)
import Data.Text   (Text)
import Data.Vector (Vector)

import qualified Data.Patch as P
import qualified Data.Text  as T

type PatchSummary = (Int, Int, Int)

patchSummary :: Patch a -> PatchSummary
patchSummary p | (Sum a, Sum b, Sum c) <- mconcat (map toCounts $ P.toList p) = (a,b,c)
  where toCounts (P.Insert  {}) = (Sum 1, Sum 0, Sum 0)
        toCounts (P.Delete  {}) = (Sum 0, Sum 1, Sum 0)
        toCounts (P.Replace {}) = (Sum 0, Sum 0, Sum 1)

patchToText :: Patch Char -> Text
patchToText = T.pack . toList . patchToVector

patchToVector :: Patch Char -> Vector Char
patchToVector = flip P.apply mempty
