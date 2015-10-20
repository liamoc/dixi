{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}
module Dixi.API where

import Data.Text (Text)
import Data.Patch
import Data.Proxy
import Servant.API
import Servant.HTML.Blaze
import Text.Hamlet (Html)

import Dixi.Config
import Dixi.Common
import Dixi.Page
import Dixi.PatchUtils

type a :| b = a :<|> b
infixr 8 :|
infixr 8 |:

(|:) :: a -> b -> a :| b
(|:) = (:<|>)


data PrettyPage = PP Renders Key Version (Page Html)
data RawPage    = RP Renders Key Version (Page Text)
data DiffPage   = DP Renders Key Version Version (Page (Hunks Char))
data History    = H  Renders Key [Page PatchSummary]

data NewBody    = NB Text (Maybe Text)
data RevReq     = DR Version Version (Maybe Text)

type HistoryAPI  = Get '[HTML] History
                :| Capture "version" Version :> VersionAPI
                :| "diff" :> QueryParam "from" Version :> QueryParam "to" Version :> Get '[HTML] DiffPage
                :| "revert" :> ReqBody '[FormUrlEncoded] RevReq :> Post '[HTML] PrettyPage
type VersionAPI  = PageViewAPI
                :| ReqBody '[FormUrlEncoded] NewBody :> Post '[HTML] PrettyPage
type PageAPI     = PageViewAPI
                :| "history" :> HistoryAPI
type PageViewAPI = Get '[HTML] PrettyPage
                :| "raw" :> Get '[HTML] RawPage
type Dixi        = Capture "page" Key :> PageAPI
                :| PageAPI

dixi :: Proxy Dixi
dixi = Proxy
