{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dixi.Config ( Config (Config, port, storage)
                   , Renders (..)
                   , defaultConfig
                   , configToRenders
                   , EndoIO (..)
                   ) where

import Control.Monad ((<=<))
import Data.Aeson
import Data.Default
import Data.Maybe (catMaybes)
import Data.Monoid (Last (..), Monoid (..))
import Data.Time (UTCTime, formatTime)
import Data.Time.Locale.Compat
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import GHC.Generics
import Text.Pandoc hiding (Format, readers)
import Text.Pandoc.Error

import Dixi.Pandoc.Wikilinks

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
            , processors :: [String]
            } deriving (Generic, Show)

instance FromJSON Config where
instance ToJSON   Config where
instance FromJSON TimeConfig where
instance ToJSON   TimeConfig where
instance FromJSON Format where
instance ToJSON   Format where

type PureReader = ReaderOptions -> String -> Either PandocError Pandoc

newtype EndoIO x = EndoIO { runEndoIO :: x -> IO x }

instance Monoid (EndoIO a) where
  mempty = EndoIO return
  EndoIO a `mappend` EndoIO b = EndoIO (a <=< b)

data Renders = Renders
   { renderTime :: Last UTCTime -> String
   , pandocReader :: PureReader
   , pandocWriterOptions :: WriterOptions
   , pandocProcessors :: EndoIO Pandoc
   }

defaultConfig :: Config
defaultConfig = Config 8000 "state"
                       (TimeConfig "Etc/UTC" "%T, %F")
                       (Format "org")
                       ("http://localhost:8000")
                       ["wikilinks"]

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

allProcessors :: Config -> [(String, EndoIO Pandoc)]
allProcessors Config {..}
              = [("wikilinks", EndoIO $ wikilinks url)
                ]

configToRenders :: Config -> IO Renders
configToRenders cfg@(Config {..}) = do
  olsonData <- getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ timezone time)
  let renderTime (Last Nothing) = "(never)"
      renderTime (Last (Just t)) = formatTime defaultTimeLocale (format time) $ utcToLocalTime' olsonData t
      pandocReader | Format f <- readerFormat 
        = case lookup f readers of
            Just r -> r
            Nothing -> readOrg
      pandocWriterOptions = def { writerSourceURL = Just url }

      pandocProcessors = mconcat $ catMaybes $ map (flip lookup $ allProcessors cfg) processors
  return $ Renders {..}
