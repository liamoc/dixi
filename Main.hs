{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Acid
import Data.Text (Text)
import Network.Wai.Handler.Warp
import Servant

import Dixi.API
import Dixi.Common
import Dixi.Database
import Dixi.Forms  () -- imported for orphans
import Dixi.Markup () -- imported for orphans
import Dixi.Page

page :: AcidState Database -> Key -> Server PageAPI
page db key =  latest
            |: history
  where
    latest  =  latestQ PP |: latestQ RP

    diffPages (Just v1) (Just v2) = do liftIO $ DP key v1 v2 <$> query db (GetDiff key (v1, v2))
    diffPages _ _ = left err400

    history =  do liftIO $ H key <$> query db (GetHistory key)
            |: version
            |: diffPages
            |: reversion

    reversion (DR v1 v2 com) = do
      _ <- liftIO $ update db $ Revert key (v1, v2) com
      latestQ PP
    version v =  (versionQ PP v |: versionQ RP v)
              |: updateVersion v
    updateVersion v (NB t c) = do _ <- liftIO $ update db $ Amend key v t c
                                  latestQ PP

    latestQ :: (Key -> Version -> Page Text -> a) -> EitherT ServantErr IO a
    latestQ pp = do liftIO $ uncurry (pp key) <$> query db (GetLatest key)

    versionQ :: (Key -> Version -> Page Text -> a) -> Version -> EitherT ServantErr IO a
    versionQ pp v = do liftIO $ pp key v <$> query db (GetVersion key v)

server :: AcidState Database -> Server Dixi
server db =  page db
          |: page db "Main_Page"

main :: IO ()
main = do
  db <- openLocalState emptyDB
  run 8081 $ serve dixi $ server db
