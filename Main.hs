{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE ConstraintKinds   #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Acid
import Data.Default
import Data.Time
import Data.Text (Text)
import Data.Traversable
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Pandoc

#ifdef OLDBASE
import Control.Applicative
#endif

import qualified Data.Yaml as Y
import qualified Data.Text as T

import Dixi.API
import Dixi.Common
import Dixi.Config
import Dixi.Database
import Dixi.Forms  () -- imported for orphans
import Dixi.Markup (writePandocError) 
import Dixi.Page

spacesToUScores :: T.Text -> T.Text
spacesToUScores = T.pack . map (\x -> if x == ' ' then '_' else x) . T.unpack

page :: AcidState Database -> Renders -> Key -> Server PageAPI
page db renders (spacesToUScores -> key)
  =  latest
  |: history
  where
    latest  =  latestQ pp |: latestQ rp

    diffPages (Just v1) (Just v2) = liftIO $ DP renders key v1 v2 <$> query db (GetDiff key (v1, v2))
    diffPages _ _ = left err400

    history =  liftIO (H renders key <$> query db (GetHistory key))
            |: version
            |: diffPages
            |: reversion

    reversion (DR v1 v2 com) = do
      _ <- liftIO (getCurrentTime >>= update db . Revert key (v1, v2) com)
      latestQ pp
    version v =  (versionQ pp v |: versionQ rp v)
              |: updateVersion v
    updateVersion v (NB t c) = do _ <- liftIO (getCurrentTime >>= update db . Amend key v t c)
                                  latestQ pp

    latestQ :: (Key -> Version -> Page Text -> IO a) -> EitherT ServantErr IO a
    latestQ p = liftIO (uncurry (p key) =<< query db (GetLatest key))

    versionQ :: (Key -> Version -> Page Text -> IO a) -> Version -> EitherT ServantErr IO a
    versionQ p v = liftIO (p key v =<< query db (GetVersion key v))

    pp :: Key -> Version -> Page Text -> IO PrettyPage
    pp k v p = fmap (PP renders k v) $ for p $ \b ->
                 case pandocReader renders def (filter (/= '\r') . T.unpack $ b) of
                   Left err -> return $ writePandocError err
                   Right pd -> writeHtml (pandocWriterOptions renders) <$> runEndoIO (pandocProcessors renders) pd

    rp k v p = return (RP renders k v p)


server :: AcidState Database -> Renders -> Server Dixi
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
         _ | i `elem` ("cC" :: String) -> Y.encodeFile x defaultConfig >> main'' (Right defaultConfig)
           | i `elem` ("rR" :: String) -> main'' (Right defaultConfig)
           | otherwise                 -> exitFailure
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
     run port . serve dixi . server db =<< configToRenders cfg
