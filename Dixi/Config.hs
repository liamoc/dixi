{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}

module Dixi.Config ( Config (Config, port, storage, static, stylesheet)
                   , Renders (..)
                   , defaultConfig
                   , configToRenders
                   , defaultRenders
                   , EndoIO (..)
                   ) where

import Control.Monad ((<=<))
import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.Default
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid
import Data.Time (UTCTime, formatTime)
import Data.Time.Locale.Compat
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import GHC.Generics
import Text.Hamlet

import qualified Text.Pandoc       as P
import qualified Text.Pandoc.Error as P

import Dixi.Pandoc.Wikilinks

newtype Format = Format String
               deriving (Generic, Show)

data TimeConfig = TimeConfig
                { timezone :: String
                , format   :: String
                } deriving (Generic, Show)



data MathMethod = Plain | LatexMathML | MathML | MathJax | Katex deriving (Generic, Show)

data Config = Config
            { port         :: Int
            , storage      :: FilePath
            , static       :: Maybe FilePath
            , stylesheet   :: FilePath
            , time         :: TimeConfig
            , readerFormat :: Format
            , url          :: String
            , processors   :: [String]
            , math         :: MathMethod
            } deriving (Generic, Show)

instance FromJSON Config     where
  parseJSON = genericParseJSON (defaultOptions {omitNothingFields = True})
instance ToJSON   Config     where
  toJSON = genericToJSON (defaultOptions {omitNothingFields = True})
instance FromJSON TimeConfig where -- generic
instance ToJSON   TimeConfig where -- generic
instance FromJSON Format     where -- generic
instance ToJSON   Format     where -- generic
instance FromJSON MathMethod where
  parseJSON = genericParseJSON (defaultOptions { constructorTagModifier = map toLower})
instance ToJSON   MathMethod where
  toJSON = genericToJSON (defaultOptions { constructorTagModifier = map toLower })

type PureReader = P.ReaderOptions -> String -> Either P.PandocError P.Pandoc

newtype EndoIO x = EndoIO { runEndoIO :: x -> IO x }

instance Monoid (EndoIO a) where
  mempty = EndoIO return
  EndoIO a `mappend` EndoIO b = EndoIO (a <=< b)

data Renders = Renders
   { renderTime :: Last UTCTime -> String
   , pandocReader :: PureReader
   , pandocWriterOptions :: P.WriterOptions
   , pandocProcessors :: EndoIO P.Pandoc
   , headerBlock :: Html
   }

defaultRenders :: Renders
defaultRenders = Renders renderTime P.readOrg def mempty
                         [shamlet|<link rel="stylesheet" href="/static/stylesheet.css">|]
  where renderTime (Last Nothing)  = "(never)"
        renderTime (Last (Just t)) = show t

defaultConfig :: Config
defaultConfig = Config 8000 "state" (Just "static") "style.css"
                       (TimeConfig "Etc/UTC" "%T, %F")
                       (Format "org")
                       "http://localhost:8000"
                       ["wikilinks"]
                       Katex

readers :: [(String, PureReader)]
readers = [ ("native"       , const P.readNative)
          , ("json"         , P.readJSON )
          , ("commonmark"   , P.readCommonMark)
          , ("rst"          , P.readRST)
          , ("markdown"     , P.readMarkdown)
          , ("mediawiki"    , P.readMediaWiki)
          , ("docbook"      , P.readDocBook)
          , ("opml"         , P.readOPML)
          , ("org"          , P.readOrg)
          , ("textile"      , P.readTextile)
          , ("html"         , P.readHtml)
          , ("latex"        , P.readLaTeX)
          , ("haddock"      , P.readHaddock)
          , ("twiki"        , P.readTWiki)
          , ("t2t"          , P.readTxt2TagsNoMacros)
          ]

allProcessors :: Config -> [(String, EndoIO P.Pandoc)]
allProcessors Config {..}
              = [ ("wikilinks", EndoIO $ wikilinks url)
                ]

convertMathMethod :: MathMethod -> P.HTMLMathMethod
convertMathMethod Plain = P.PlainMath
convertMathMethod LatexMathML = P.LaTeXMathML Nothing
convertMathMethod MathML = P.MathML Nothing
convertMathMethod MathJax = P.MathJax ""
convertMathMethod Katex = P.KaTeX "" ""

headerForMathMethod :: MathMethod -> Html
headerForMathMethod Plain = [shamlet||]
headerForMathMethod LatexMathML = [shamlet|
     <script type="text/javascript" src="http://math.etsu.edu/LaTeXMathML/LaTeXMathML.js">
     <link rel="stylesheet" type="text/css" href="http://math.etsu.edu/LaTeXMathML/LaTeXMathML.standardarticle.css">
   |]
headerForMathMethod MathML = [shamlet||]
headerForMathMethod Katex = [shamlet|
     <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/KaTeX/0.5.1/katex.min.css">
     <script src="//cdnjs.cloudflare.com/ajax/libs/KaTeX/0.5.1/katex.min.js">
     <script src="//code.jquery.com/jquery-1.11.2.min.js"></script>
     <script> $(function () { $(".math").each(function (x) { katex.render($(this).text(), this); }); });
   |]
headerForMathMethod MathJax = [shamlet|
     <script type="text/javascript" src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
   |]

configToRenders :: Config -> IO Renders
configToRenders cfg@(Config {..}) = do
  olsonData <- getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ timezone time)
  let renderTime (Last Nothing) = "(never)"
      renderTime (Last (Just t)) = formatTime defaultTimeLocale (format time) $ utcToLocalTime' olsonData t
      pandocReader | Format f <- readerFormat = fromMaybe P.readOrg (lookup f readers)
      pandocWriterOptions = def { P.writerSourceURL = Just url, P.writerHTMLMathMethod = convertMathMethod math }
      pandocProcessors = mconcat $ mapMaybe (flip lookup $ allProcessors cfg) processors
      stylesheetUrl = url ++ "/static/" ++ stylesheet
      headerBlock = [shamlet|
                       <link rel="stylesheet" href="#{stylesheetUrl}">
                       #{headerForMathMethod math}
                    |]
  return Renders {..}
