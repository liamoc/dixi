{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}
module Dixi.API where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Text (Text)
import Data.Patch (Hunks, HunkStatus (..))
import Data.Proxy
import Data.Vector (Vector)
import Servant.API
import Servant.HTML.Blaze
import Text.Blaze.Html.Renderer.Text
import Text.Hamlet (Html)

#ifdef OLDBASE
import Control.Applicative
#endif

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

type HistoryAPI  = Get '[HTML, JSON] History
                :| Capture "version" Version :> VersionAPI
                :| "diff" :> QueryParam "from" Version :> QueryParam "to" Version :> Get '[HTML, JSON] DiffPage
                :| "revert" :> ReqBody '[FormUrlEncoded, JSON] RevReq :> Post '[HTML, JSON] PrettyPage
type VersionAPI  = PageViewAPI
                :| ReqBody '[FormUrlEncoded, JSON] NewBody :> Post '[HTML, JSON] PrettyPage
type PageAPI     = PageViewAPI
                :| "history" :> HistoryAPI
type PageViewAPI = Get '[HTML, JSON] PrettyPage
                :| "raw" :> Get '[HTML, JSON] RawPage
type Dixi        = Capture "page" Key :> PageAPI
                :| PageAPI


instance FromJSON RevReq where
  parseJSON (Object o) = DR <$> o .: "from" <*> o .: "to" <*> o .:? "comment"
  parseJSON wat        = typeMismatch "Revert" wat

instance ToJSON RevReq where
  toJSON (DR v1 v2 Nothing) = object ["from" .= v1, "to" .= v2]
  toJSON (DR v1 v2 (Just c)) = object ["from" .= v1, "to" .= v2, "comment" .= c]

instance FromJSON NewBody where
  parseJSON (Object o) = NB <$> o .: "content" <*> o .:? "comment"
  parseJSON wat        = typeMismatch "NewBody" wat

instance ToJSON NewBody where
  toJSON (NB cn Nothing)  = object ["content" .= cn ]
  toJSON (NB cn (Just c)) = object ["content" .= cn, "comment" .= c ]


instance ToJSON DiffPage where
  toJSON (DP (Renders {..}) k v1 v2 p)
      = object [ "title" .= k
               , "versions" .= object [ "from" .= v1 , "to" .= v2 ]
               , "diff" .= map (uncurry hunkToJSON) (p ^. body)
               ]
    where
      hunkToJSON :: Vector Char -> HunkStatus -> Value 
      hunkToJSON v s = object [ "text" .= toList v
                              , "status" .= case s of Inserted  -> '+'
                                                      Deleted   -> '-'
                                                      Replaced  -> '~'
                                                      Unchanged -> ' '
                              ]
instance ToJSON RawPage where
  toJSON (RP (Renders {..}) k v p)
    = let tim = renderTime $ p ^. time
          com = p ^. comment . traverse
      in object [ "title"   .= k
                , "version" .= v
                , "time"    .= tim
                , "comment" .= com
                , "content" .= (p ^. body)
                ]


instance ToJSON PrettyPage where
  toJSON (PP (Renders {..}) k v p)
    = let tim = renderTime $ p ^. time
          com = p ^. comment . traverse
      in object [ "title"   .= k
                , "version" .= v
                , "time"    .= tim
                , "comment" .= com
                , "content" .= renderHtml (p ^. body)
                ]

instance ToJSON History where
  toJSON (H (Renders {..}) k cs) = object [ "title" .= k , "history" .= zipWith versionToJSON [1 :: Version ..] cs]
    where
      versionToJSON v p = let
          tim = renderTime $ p ^. time
          com = p ^. comment . traverse
          (a,b,c) = p ^. body
        in object [ "version" .= v
                  , "time" .= tim
                  , "comment" .= com
                  , "changes" .= object [ "insertions" .= a , "deletions" .= b, "modifications" .= c]
                  ]


dixi :: Proxy Dixi
dixi = Proxy
