{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE CPP                #-}
{-# OPTIONS -fno-warn-orphans   #-}
module Dixi.Database
       ( Database , emptyDB
       , DixiError (..)
       , Amend (..)
       , GetDiff (..)
       , GetHistory (..)
       , GetLatest (..)
       , GetVersion (..)
       , Revert (..)
       ) where

import Control.Lens
import Control.Monad.Trans.Except
import Control.Monad
import Data.Acid
import Data.Compositions.Snoc (Compositions)
import Data.Foldable
import Data.Map (Map)
import Data.Monoid
import Data.Patch (Patch)
import Data.SafeCopy hiding (Version)
import Data.Text (Text)
import Data.Time
import Data.Typeable

#ifdef OLDBASE
import Control.Applicative
#endif

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

deriveSafeCopy 0 'base ''DixiError

makeLenses ''Database


getLatest :: Key -> Query Database (Version, Page Text)
getLatest k = do
  b <- view (db . ix k)
  return (C.length b, patchToText <$> C.composed b)

getVersion :: Key -> Version -> Query Database (Either DixiError (Page Text))
getVersion k v = runExceptT $ do
  b <- view (db . ix k)
  when (C.length b < v) $ throwE $ VersionNotFound k v
  return $ patchToText <$> C.composed (C.take v b)

getHistory :: Key -> Query Database [Page PatchSummary]
getHistory k = view (db . ix k . to (fmap (fmap patchSummary) . toList))

{-# ANN module "HLint: ignore Reduce duplication" #-}
getDiff :: Key -> (Version, Version) -> Query Database (Either DixiError (Page (P.Hunks Char)))
getDiff k (v1 , v2) | v1 > v2   = getDiff k (v2, v1)
                    | otherwise = runExceptT $ do
  b <- view (db . ix k)
  let latest = C.length b
  when (latest < v1) $ throwE $ VersionNotFound k v1
  when (latest < v2) $ throwE $ VersionNotFound k v2
  let y = patchToVector (C.composed (C.take v1 b) ^. body)
  return $ fmap (`P.hunks` y) (C.dropComposed v1 (C.take v2 b)) 

amendPatch :: Key -> Version -> Patch Char -> Maybe Text -> UTCTime -> Update Database (Either DixiError Version)
amendPatch k v q com tim = runExceptT $ do
  b <- use (db . ix k)
  let latest = C.length b
  when (latest < v) $ throwE $ VersionNotFound k v
  let p = C.dropComposed v b ^. body
      r = snd $ P.transformWith P.theirs p q
  (db . at k) .= Just (C.snoc b (Page r (Last com) (Last $ Just tim)))
  return (latest + 1)

amend :: Key -> Version -> Text -> Maybe Text -> UTCTime -> Update Database (Either DixiError Version)
amend k v new com tim = runExceptT $ do
  b <- use (db . ix k)
  when (C.length b < v) $ throwE $ VersionNotFound k v
  let n = V.fromList (T.unpack new)
      o = C.composed (C.take v b) ^. body . to patchToVector
      q = P.diff o n
  ExceptT $ amendPatch k v q com tim


revert :: Key -> (Version, Version) -> Maybe Text -> UTCTime -> Update Database (Either DixiError Version)
revert k (v1,v2) com tim | v2 < v1 = revert k (v2, v1) com tim
revert k (v1,v2) com tim = runExceptT $ do
   b <- use (db . ix k)
   let latest = C.length b
   when (latest < v1) $ throwE $ VersionNotFound k v1
   when (latest < v2) $ throwE $ VersionNotFound k v2
   let p = C.dropComposed v1 (C.take v2 b) ^. body
   ExceptT $ amendPatch k (max v1 v2) (P.inverse p) com tim

makeAcidic ''Database ['getLatest, 'getVersion, 'getHistory, 'getDiff, 'amend, 'revert]


