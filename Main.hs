{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Acid
import Data.Time
import Data.Text (Text)
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.Environment
import System.Exit
import System.IO

import qualified Data.Yaml as Y
import qualified Data.Text as T

import Dixi.API
import Dixi.Common
import Dixi.Config
import Dixi.Database
import Dixi.Forms  () -- imported for orphans
import Dixi.Markup () -- imported for orphans
import Dixi.Page

spacesToUScores :: T.Text -> T.Text
spacesToUScores = T.pack . map (\x -> if x == ' ' then '_' else x) . T.unpack

page :: AcidState Database -> Config -> Key -> Server PageAPI
page db cfg (spacesToUScores -> key)
  =  latest
  |: history
  where
    latest  =  latestQ (PP renders) |: latestQ (RP renders)

    diffPages (Just v1) (Just v2) = do liftIO $ DP renders key v1 v2 <$> query db (GetDiff key (v1, v2))
    diffPages _ _ = left err400

    history =  do liftIO $ H renders key <$> query db (GetHistory key)
            |: version
            |: diffPages
            |: reversion

    reversion (DR v1 v2 com) = do
      _ <- liftIO (getCurrentTime >>= update db . Revert key (v1, v2) com)
      latestQ (PP renders)
    version v =  (versionQ (PP renders) v |: versionQ (RP renders) v)
              |: updateVersion v
    updateVersion v (NB t c) = do _ <- liftIO $ (getCurrentTime >>= update db . Amend key v t c)
                                  latestQ (PP renders)

    latestQ :: (Key -> Version -> Page Text -> a) -> EitherT ServantErr IO a
    latestQ pp = do liftIO $ uncurry (pp key) <$> query db (GetLatest key)

    versionQ :: (Key -> Version -> Page Text -> a) -> Version -> EitherT ServantErr IO a
    versionQ pp v = do liftIO $ pp key v <$> query db (GetVersion key v)

    renders = configToRenders cfg

server :: AcidState Database -> Config -> Server Dixi
server db cfg =  page db cfg
              |: page db cfg "Main_Page"

main :: IO ()
main = getArgs >>= main'
 where
   main' [] = main' ["config.yml"]
   main' [x] = do
     c <- doesFileExist x
     if c then Y.decodeFileEither x >>= main''
          else do
       putStrLn "Configuration file not found, would you like to [c]reate one, [r]un with default configuration or e[x]it?"
       hSetBuffering stdin NoBuffering
       getChar >>= \i -> case i of
         _ | i `elem` ['c','C'] -> Y.encodeFile x defaultConfig >> main'' (Right defaultConfig)
         _ | i `elem` ['r','R'] -> main'' (Right defaultConfig)
         _ | otherwise          -> exitFailure
   main' _ = do
     p <- getProgName
     hPutStrLn stderr $ "usage:\n   " ++ p ++ " [/path/to/config.yml]"
     exitFailure
   main'' (Left e) = do
     hPutStrLn stderr $ Y.prettyPrintParseException e
     exitFailure
   main'' (Right cfg@(Config {..})) = do
     db <- openLocalStateFrom storage emptyDB
     createCheckpoint db
     createArchive    db
     run port $ serve dixi $ server db cfg
