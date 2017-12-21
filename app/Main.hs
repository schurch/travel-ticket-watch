{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Text.Lazy (fromStrict)
import DataAccess
import KiwiAPI
import System.Environment
import System.Exit
import System.IO
import Text.Mustache
import Web.Scotty

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, configPath] ->
      case mode of
        "W" -> runWebServer configPath
        "R" -> refreshSearches configPath
        _ -> failAndExit
    _ -> do
      failAndExit

failAndExit :: IO ()
failAndExit = do
  name <- getProgName
  hPutStrLn stderr $
    "usage: " ++ name ++ " run-mode[W=Web,R=Refresher] config-path"
  exitFailure

refreshSearches :: String -> IO ()
refreshSearches configPath = do
  config <- (decodeConfig configPath)
  let connectInfo = connectionInfo $ (databaseConfig config)
  let outdatedInterval = (outdatedIntervalSeconds $ appConfig config)
  let endpoint = (kiwiEndpoint $ kiwiConfig config)
  searches <- fetchSearches connectInfo outdatedInterval
  forM_ searches (updatePriceForSearch connectInfo endpoint)

runWebServer :: String -> IO ()
runWebServer configPath = do
  config <- (decodeConfig configPath)
  let searchSpace = [".", "./templates"]
  let templateName = "flights.mustache"
  compiled <- automaticCompile searchSpace templateName
  case compiled of
    Left err -> print err
    Right template -> runScotty config template

runScotty :: Config -> Template -> IO ()
runScotty config template = do
  scotty 3000 $ do
    get "/flights/:id" $ do
      let connectInfo = connectionInfo $ (databaseConfig config)
      searchId <- param "id"
      flights <- liftIO $ fetchFlightsForSearch connectInfo searchId
      html $ fromStrict $ substitute template flights
      -- json flights

staticPath :: String -> ScottyM ()
staticPath path =
  let routePattern = capture ("/" ++ path ++ "/:file")
  in get routePattern $ do
       fileName <- param "file"
       file $ "./" ++ path ++ "/" ++ fileName
