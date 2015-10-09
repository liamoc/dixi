{-# LANGUAGE DeriveGeneric #-}

module Dixi.Config where

import Data.Aeson
import GHC.Generics

data Config = Config
            { port :: Int
            , storage :: FilePath
            } deriving (Generic, Show)

instance FromJSON Config where
instance ToJSON   Config where

defaultConfig :: Config
defaultConfig = Config 8000 "state"
