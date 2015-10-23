{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS -fno-warn-orphans  #-}
module Dixi.Forms where

import Servant.API
import Text.Read

#ifdef OLDBASE
import Control.Applicative
#endif

import qualified Data.Text as T

import Dixi.API

instance FromFormUrlEncoded NewBody where
  fromFormUrlEncoded x = NB <$> maybe (Left "error") Right (lookup "content" x) <*> pure (lookup "comment" x)
instance ToFormUrlEncoded NewBody where
  toFormUrlEncoded (NB t c) = ("content", T.pack $ show t) : maybe [] (pure . ("comment",)) c
instance ToFormUrlEncoded RevReq where
  toFormUrlEncoded (DR v1 v2 c) = [("from", T.pack $ show v1), ("to", T.pack $ show v2)] ++ maybe [] (pure . ("comment",)) c
instance FromFormUrlEncoded RevReq where
  fromFormUrlEncoded x = DR <$> maybe (Left "error") Right (lookup "from" x >>= readMaybe . T.unpack)
                            <*> maybe (Left "error") Right (lookup "to" x   >>= readMaybe . T.unpack)
                            <*> pure (lookup "comment" x)
