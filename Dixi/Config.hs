{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dixi.Config ( Config (Config, port, storage)
                   , Renders (..)
                   , defaultConfig
                   , configToRenders
                   ) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Time (UTCTime, formatTime)
import Data.Time.Locale.Compat
import Data.Time.Zones.All (toTZName, fromTZName, tzByLabel, TZLabel (..))
import Data.Time.Zones (utcToLocalTimeTZ)
import GHC.Generics
import qualified Data.ByteString.Char8 as B

data TimeConfig = TimeConfig
            { timezone :: String
            , format :: String
            } deriving (Generic, Show)
data Config = Config
            { port :: Int
            , storage :: FilePath
            , time :: TimeConfig
            } deriving (Generic, Show)

instance FromJSON Config where
instance ToJSON   Config where
instance FromJSON TimeConfig where
instance ToJSON   TimeConfig where

data Renders = Renders
   { renderTime :: Last UTCTime -> String }

defaultConfig :: Config
defaultConfig = Config 8000 "state" (TimeConfig (B.unpack $ toTZName Etc__UTC) "%T, %F")


configToRenders :: Config -> Renders
configToRenders Config {..} = Renders {..}
  where renderTime (Last Nothing) = "(never)"
        renderTime (Last (Just t)) = let label = fromMaybe Etc__UTC (fromTZName $ B.pack $ timezone time)
                                      in formatTime defaultTimeLocale (format time) $ utcToLocalTimeTZ (tzByLabel label) t
