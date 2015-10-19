{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dixi.Config ( Config (Config, port, storage)
                   , Renders (..)
                   , defaultConfig
                   , configToRenders
                   ) where

import Data.Aeson
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Time (UTCTime, formatTime)
import Data.Time.Locale.Compat
import Data.Time.Zones.All (toTZName, fromTZName, tzByLabel, TZLabel (..))
import Data.Time.Zones (utcToLocalTimeTZ)
import GHC.Generics
import Text.Pandoc hiding (Format, readers)
import Text.Pandoc.Error
import qualified Data.ByteString.Char8 as B


newtype Format = Format String deriving (Generic, Show)

data TimeConfig = TimeConfig
            { timezone :: String
            , format :: String
            } deriving (Generic, Show)
data Config = Config
            { port :: Int
            , storage :: FilePath
            , time :: TimeConfig
            , readerFormat :: Format
            , url :: String
            } deriving (Generic, Show)

instance FromJSON Config where
instance ToJSON   Config where
instance FromJSON TimeConfig where
instance ToJSON   TimeConfig where
instance FromJSON Format where
instance ToJSON   Format where

type PureReader = ReaderOptions -> String -> Either PandocError Pandoc

data Renders = Renders
   { renderTime :: Last UTCTime -> String
   , pandocReader :: PureReader
   , pandocWriterOptions :: WriterOptions
   }

defaultConfig :: Config
defaultConfig = Config 8000 "state" (TimeConfig (B.unpack $ toTZName Etc__UTC) "%T, %F") (Format "org") ("http://localhost:8000/")

readers :: [(String, PureReader)]
readers = [ ("native"       , const readNative)
           ,("json"         , readJSON )
           ,("commonmark"   , readCommonMark)
           ,("rst"          , readRST)
           ,("markdown"     , readMarkdown)
           ,("mediawiki"    , readMediaWiki)
           ,("docbook"      , readDocBook)
           ,("opml"         , readOPML)
           ,("org"          , readOrg)
           ,("textile"      , readTextile) 
           ,("html"         , readHtml)
           ,("latex"        , readLaTeX)
           ,("haddock"      , readHaddock)
           ,("twiki"        , readTWiki)
           ,("t2t"          , readTxt2TagsNoMacros)
           ]
configToRenders :: Config -> Renders
configToRenders Config {..} = Renders {..}
  where renderTime (Last Nothing) = "(never)"
        renderTime (Last (Just t)) = let label = fromMaybe Etc__UTC (fromTZName $ B.pack $ timezone time)
                                      in formatTime defaultTimeLocale (format time) $ utcToLocalTimeTZ (tzByLabel label) t
        pandocReader | Format f <- readerFormat 
          = case lookup f readers of
              Just r -> r
              Nothing -> readOrg
        pandocWriterOptions = def { writerSourceURL = Just url }
