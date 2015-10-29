{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}

import Control.Exception(bracket)
import Control.Monad
import Data.Acid
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.IO

#ifdef OLDBASE
import Control.Applicative
#endif

import qualified Data.Yaml         as Y
import qualified Data.Text.Lazy.IO as L

import Dixi.API
import Dixi.Config
import Dixi.Database
import Dixi.Forms  () -- imported for orphans
import Dixi.Server
import Dixi.Markup (defaultStylesheet)

getChar' :: IO Char
getChar' =
  bracket (hGetBuffering stdin)
          (hSetBuffering stdin)
          $ const $ do
             hSetBuffering stdin NoBuffering
             bracket (hGetEcho stdin)
                     (hSetEcho stdin)
                     $ const $ hSetEcho stdin False >> getChar

handleStatics, handleStylesheet :: FilePath -> IO ()

handleStatics fn = do
  c <- doesDirectoryExist fn
  unless c $ do
    putStrLn "Statics directory not found, would you like to [c]reate it, or e[x]it?"
    getChar' >>= \i -> case i of
      _ | i `elem` ("cC" :: String) -> createDirectory fn
        | otherwise                 -> exitFailure

handleStylesheet fn = do
  c <- doesFileExist fn
  unless c $ do
    putStrLn "Stylesheet file not found, would you like to [c]reate one, or e[x]it?"
    getChar' >>= \i -> case i of
      _ | i `elem` ("cC" :: String) -> L.writeFile fn defaultStylesheet
        | otherwise                 -> exitFailure

handleConfig :: FilePath -> IO (Either Y.ParseException Config)
handleConfig fn = do
  c <- doesFileExist fn
  if c then Y.decodeFileEither fn
       else do
    putStrLn "Configuration file not found, would you like to [c]reate one, [r]un with default configuration or e[x]it?"
    getChar' >>= \i -> case i of
      _ | i `elem` ("cC" :: String) -> Y.encodeFile fn defaultConfig >> return (Right defaultConfig)
        | i `elem` ("rR" :: String) -> return (Right defaultConfig)
        | otherwise                 -> exitFailure


dixiWithStatic :: Proxy (Dixi :| "static" :> Raw)
dixiWithStatic =  Proxy

main :: IO ()
main = getArgs >>= main'
 where
   main' []  = main' ["config.yaml"]
   main' [x] = handleConfig x >>= main''
   main' _ = do
     p <- getProgName
     hPutStrLn stderr $ "usage:\n   " ++ p ++ " [/path/to/config.yaml]"
     exitFailure
   main'' (Left e) = do
     hPutStrLn stderr $ Y.prettyPrintParseException e
     exitFailure
   main'' (Right cfg@(Config {..})) = do
     case static of
       Just st -> do
         handleStatics st
         handleStylesheet (st </> stylesheet)
       Nothing -> return ()
     db <- openLocalStateFrom storage emptyDB
     createCheckpoint db
     createArchive    db
     rs <- configToRenders cfg
     putStrLn "Starting dixi"
     run port $ case static of
       Just st -> serve dixiWithStatic $ server db rs |: serveDirectory st
       Nothing -> serve dixi $ server db rs
