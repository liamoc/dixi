{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RankNTypes         #-}
module Dixi.Database
       ( Database , emptyDB
       , Amend (..)
       , GetDiff (..)
       , GetHistory (..)
       , GetLatest (..)
       , GetVersion (..)
       , Revert (..)
       ) where

import Control.Applicative
import Control.Lens
import Data.Acid
import Data.Compositions.Snoc (Compositions)
import Data.Foldable
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Patch (Patch)
import Data.SafeCopy hiding (Version)
import Data.Text (Text)
import Data.Typeable

import qualified Data.Compositions.Snoc as C
import qualified Data.Patch             as P
import qualified Data.Text              as T
import qualified Data.Vector            as V

import Dixi.Common
import Dixi.Database.Orphans ()
import Dixi.Page
import Dixi.PatchUtils

data Database = DB { _db :: Map Key (Compositions (Page (Patch Char)))}
                deriving (Typeable)

emptyDB :: Database
emptyDB = DB mempty

deriveSafeCopy 0 'base ''Database

makeLenses ''Database

getLatest :: Key -> Query Database (Version, Page Text)
getLatest k = do
  b <- (view (db . ix k))
  return (C.length b, patchToText <$> C.composed b)

getVersion :: Key -> Version -> Query Database (Page Text)
getVersion k v = do
  b <- view (db . ix k)
  return $ patchToText <$> C.composed (C.take v b)

getHistory :: Key -> Query Database [Page PatchSummary]
getHistory k = view (db . ix k . to (fmap (fmap patchSummary) . toList))

getDiff :: Key -> (Version, Version) -> Query Database (Page (P.Hunks Char))
getDiff k (v1 , v2) | v1 > v2   = getDiff k (v2, v1)
                    | otherwise = do
  b <- view (db . ix k)
  let y = patchToVector (C.composed (C.take v1 b) ^. body)
  return $ fmap (\z -> P.hunks z y) (C.dropComposed v1 (C.take v2 b)) 

amendPatch :: Key -> Version -> Patch Char -> Maybe Text -> Update Database (Version)
amendPatch k v q com = do
  (db . at k) %= \mb ->
    let b = fromMaybe mempty mb
        p = C.dropComposed v b ^. body
        r = snd $ P.transformWith P.theirs p q
     in Just (C.snoc b (Page r (Last com)))
  C.length <$> use (db . ix k)

amend :: Key -> Version -> Text -> Maybe Text -> Update Database (Version)
amend k v new com = do
  b <- use (db . ix k)
  let n = V.fromList (T.unpack new)
      o = C.composed (C.take v b) ^. body . to patchToVector
      q = P.diff o n
  amendPatch k v q com


revert :: Key -> (Version, Version) -> Maybe Text -> Update Database Version
revert k (v1,v2) com | v2 < v1 = revert k (v2, v1) com
revert k (v1,v2) com = do
   b <- use (db . ix k)
   let p = C.dropComposed v1 (C.take v2 b) ^. body
   amendPatch k (max v1 v2) (P.inverse p) com

makeAcidic ''Database ['getLatest, 'getVersion, 'getHistory, 'getDiff, 'amend, 'revert]


