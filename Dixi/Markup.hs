{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-orphans #-}
module Dixi.Markup where

import Control.Lens
import Data.Foldable (toList)
import Data.Maybe    (fromMaybe)
import Data.Monoid
import Data.Patch    (Hunks, HunkStatus(..))
import Data.Proxy
import Data.Text     (Text)
import Servant.API
import Servant.HTML.Blaze
import Text.Blaze
import Text.Hamlet   (shamlet, Html)
import Text.Heredoc
import Text.Lucius
import Text.Pandoc.Error

import qualified Data.Text      as T
import qualified Data.Text.Lazy as L

import Dixi.API
import Dixi.Common
import Dixi.Config
import Dixi.Page
import Dixi.Hamlet
import Dixi.PatchUtils

link :: (IsElem endpoint Dixi, HasLink endpoint) => Proxy endpoint -> MkLink endpoint
link = safeLink dixi



renderTitle :: Text -> Text
renderTitle = T.pack . map (\c -> if c == '_' then ' ' else c) .  T.unpack

prettyUrl :: Proxy (  Capture "page" Key :> "history"
                   :> Capture "version" Version
                   :> Get '[HTML, JSON] PrettyPage
                   )
prettyUrl =  Proxy

latestUrl :: Proxy (Capture "page" Key :> Get '[HTML, JSON] PrettyPage)
latestUrl =  Proxy

rawUrl :: Proxy (  Capture "page" Key :> "history"
                :> Capture "version" Version
                :> "raw" :> Get '[HTML, JSON] RawPage
                )
rawUrl =  Proxy


amendUrl :: Proxy (  Capture "page" Key :> "history"
                  :> Capture "version" Version
                  :> ReqBody '[FormUrlEncoded, JSON] NewBody
                  :> Post '[HTML, JSON] PrettyPage
                  )
amendUrl =  Proxy

diffUrl :: Proxy (Capture "page" Key :> "history" :> "diff" :> Get '[HTML, JSON] DiffPage)
diffUrl =  Proxy
historyUrl :: Proxy (Capture "page" Key :> "history" :> Get '[HTML, JSON] History)
historyUrl =  Proxy
revertUrl :: Proxy (Capture "page" Key :> "history" :> "revert" :>  ReqBody '[FormUrlEncoded, JSON] RevReq :> Post '[HTML, JSON] PrettyPage)
revertUrl =  Proxy

outerMatter :: Html -> Text -> Html -> Html
outerMatter ss title bod = [shamlet|
  $doctype 5
  <html>
    <head>
      <link href="http://fonts.googleapis.com/css?family=PT+Serif:400,700" rel="stylesheet" type="text/css">
      <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css">
      #{ss}
      <title> #{title}
    <body>
      <div .header> #{title}
      #{bod}
|]



unlast :: a -> Last a -> a
unlast d (Last x) = fromMaybe d x

guardText :: Text -> Text -> Text
guardText x y | y == ""   = x
              | otherwise = y

instance ToMarkup URI where
  toMarkup u = [shamlet|#{show u}|]

instance ToMarkup PatchSummary where
  toMarkup (i,d,r) = [hml|
    <span .fa .fa-plus-square-o .addition-sum> #{show i}
    <span .fa .fa-minus-square-o .subtraction-sum> #{show d}
    <span .fa .fa-pencil-square-o .replacement-sum> #{show r}
|]


instance ToMarkup DiffPage where
  toMarkup (DP (Renders {..}) k v1 v2 p) = outerMatter headerBlock (renderTitle k) $ [shamlet| 
    #{pageHeader k vString}
    <div .body>
      <div>
        #{renderHunks d}
      <br>
      <hr>
      <form method="POST" action="/#{link revertUrl k}">
        <input type="hidden" name="from" value="#{show v1}">
        <input type="hidden" name="to" value="#{show v2}">
        <input type="text" name="comment" value="revert #{show v1} - #{show v2}">
        <button type="submit">
          <span .fa .fa-undo>  Revert
    |]
    where
      d = p ^. body
      renderHunks :: Hunks Char -> Html
      renderHunks ps = [hml|
        $forall (x, s) <- ps
          <span class="hunk #{styleFor s}">#{toList x}
        |]
      styleFor :: HunkStatus -> String
      styleFor Inserted  = "hunk-inserted"
      styleFor Deleted   = "hunk-deleted"
      styleFor Replaced  = "hunk-replaced"
      styleFor Unchanged = "hunk-unchanged"
      vString :: Text
      vString = "diff " <> T.pack (show v1) <> " - " <> T.pack (show v2)

instance ToMarkup History where
  toMarkup (H (Renders {..}) k []) = outerMatter headerBlock (renderTitle k) $ pageHeader k "history"
  toMarkup (H (Renders {..}) k ps) = outerMatter headerBlock (renderTitle k) $ [shamlet| 
    #{pageHeader k "history"}
    <div .body>
     <form method="GET" action="/#{link diffUrl k}">
       <table .history>
           <tr>
             <th .histh-version> Version
             <th .histh-fromto> From/To
             <th .histh-changes> Changes
             <th .histh-comment> Comment
             <th .histh-comment> Time
         $forall (v, p) <- ps'
           <tr>
            <td .hist-version>
             #{show v}.
            <td .hist-fromto>
             <input type="radio" checked style="position:relative; top:1em;" name="from" value="#{show (v - 1)}">
             <input type="radio" checked name="to" value="#{show v}">
            <td>
             #{(p ^. body)}
            <td>
             <a .histlink href="/#{link prettyUrl k v}">#{guardText "no comment" (unlast "no comment" (p ^. comment))}
            <td>
             <span .timestamp>#{renderTime (p ^. time)}
         <tr>
           <td> &nbsp;
         <tr>
           <td>
           <td>
             <button type="submit">
               <span .fa .fa-files-o>
                \  Diff
           <td>
           <td>
             <small> (to revert a change, view the diff first)
    |]
   where ps' = reverse $ zip [1..] ps

versionHeader :: Key -> Version -> Text -> Html
versionHeader k v com = [shamlet| 
           <div .subtitle>
             version #{v} (#{com'})
           <div .toolbar>
             <a href="/#{link rawUrl k v}" .fa .fa-edit> edit
             <a href="/#{link prettyUrl k v}" .fa .fa-eye> view
             <a href="/#{link historyUrl k}" .fa .fa-history> history
             <a href="/#{link latestUrl k}" .fa .fa-fast-forward> latest
  |]
 where com' = if com == "" then "no comment" else com
pageHeader :: Key -> Text -> Html
pageHeader k com = [shamlet| 
           <div .subtitle>
             #{com}
           <div .toolbar>
             <a href="/#{link historyUrl k}" .fa .fa-history> history
             <a href="/#{link latestUrl k}" .fa .fa-fast-forward> latest
  |]

instance ToMarkup PandocError where
  toMarkup (ParseFailure s)  = [shamlet| <b> Parse Failure: </b> #{s}|]
  toMarkup (ParsecError _ e) = [shamlet| <b> Parse Error: </b> #{show e} |]

writePandocError :: PandocError -> Html
writePandocError err = [shamlet|#{err}|]

instance ToMarkup PrettyPage where
  toMarkup (PP (Renders {..}) k v p)
    = let
       com = p ^. comment . traverse
       tim = renderTime $ p ^. time
    in outerMatter headerBlock (renderTitle k)
         [shamlet|
           #{versionHeader k v com}
           <div .body>
             #{p ^. body}
           <div .timestamp> This version was last edited at #{tim}
         |]

instance ToMarkup RawPage where
  toMarkup (RP (Renders {..}) k v p )
    = let
       com = p ^. comment . traverse
       bod = p ^. body
    in outerMatter headerBlock (renderTitle k)
         [shamlet|
           #{versionHeader k v com}
           <div .body>
            <form method="POST" action="/#{link amendUrl k v}">
              <textarea name="content" cols=80 rows=24 style="font-family:monospace">#{bod}
              <br>
              <input type="text" name="comment" value="no comment">
              <input type="submit">
         |]

defaultStylesheet :: L.Text
Right defaultStylesheet = luciusRT [here|
  div.body {
    margin: 1em;
  }
  table.history {
    border: 0px;
    td {
      border: 0px;
      button {
        width: 100%;
        padding: 4px;
      }
    }
    tr {
      border: 0px;
    }
  }
  .hist-version {
    text-align:right;
  }
  .histh-comment {
    text-align:left;
  }
  .histh-version {
    padding-right:5px;
  }
  .hist-fromto {
    text-align:center;
  }
  body {
    font-family: PT Serif, Palatino, Georgia, Times, serif;
    margin: 0px;
  }
  .toolbar {
    background: #BBBBAA;
    border-top: 1px solid #888877;
    border-bottom: 1px solid #EEEEDD;
    a:hover {
      background: #F1F1D9;
      border: 1px outset #F1F1D9;
    }
    a:active {
      background: #F1F1D9;
      border: 1px inset #F1F1D9;
    }
    a {
      background: #DCDCCB;
      border: 1px outset #F1F1D9;
      text-decoration: none;
      color: black;
      padding: 2px;
      margin-top: 2px;
      margin-bottom: 2px;
      margin-left: 2px;
    }
  }
  .header {
    background: #FFFFDD;
    font-size: 1.5em;
    font-weight: bold;
    padding-left: 0.5em;
    padding-top: 0.5em;
    padding-bottom: 0.5em;
  }
  .subtitle {
    float:right;
    font-size: 0.8em;
    margin-right: 0.5em;
    color: gray;
    position: relative;
    top: -2.5em;
  }
  .addition-sum {
    background: #B5F386;
    padding: 3px;
    border-radius: 6px 0px 0px 6px;
    margin-top:1px;
    margin-bottom:1px;
  }
  .subtraction-sum {
    background: #EC8160;
    padding: 3px;
    margin-top:1px;
    margin-bottom:1px;
  }
  .replacement-sum {
    background: #F3E686;
    padding: 3px;
    border-radius: 0px 6px 6px 0px;
    margin-top:1px;
    margin-bottom:1px;
  }
  .hunk {
    white-space: pre;
    font-family:monospace;
    border-radius: 4px;
  }
  .hunk-inserted {
    background: #B5F386;
  }
  .hunk-deleted {
    background: #EC8160;
    text-decoration: line-through;;
  }
  .hunk-replaced {
    background: #F3E686;
  }
  .timestamp {
    color: #444444;
    font-size: small;
  }
  div.timestamp {
    margin-left: 0.5em;
    margin-top: 2em;
  }
|] [] 
