{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans  #-}
module Dixi.Forms where

import Control.Applicative
import Servant.API
import Text.Read

import qualified Data.Text as T

import Dixi.API

instance FromFormUrlEncoded NewBody where
  fromFormUrlEncoded x = NB <$> maybe (Left "error") Right (lookup "body" x) <*> pure (lookup "comment" x)

instance FromFormUrlEncoded RevReq where
  fromFormUrlEncoded x = DR <$> maybe (Left "error") Right (lookup "from" x >>= readMaybe . T.unpack)
                            <*> maybe (Left "error") Right (lookup "to" x   >>= readMaybe . T.unpack)
                            <*> pure (lookup "comment" x)
