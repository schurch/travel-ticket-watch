{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad.Reader
import Data.List.Split (splitOn)
import Data.Text (pack)
import Data.Text.Lazy (Text, fromStrict)
import Data.Time.Calendar (Day, fromGregorian)
import DataAccess
import Database.MySQL.Simple (ConnectInfo)
import KiwiAPI
import Lucid (renderText, p_)
import Pages
import System.Environment
import System.Exit
import System.IO
import Types
import Web.Scotty.Trans
       (ScottyT, ActionT, get, scottyT, html, param, capture, file,
        redirect, post)

type App = ScottyT Text (ReaderT Config IO)

type Action = ActionT Text (ReaderT Config IO)

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
    "usage: " ++ name ++ " run-mode[W=Web,R=Refresh] config-path"
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
  scottyT (webServerPort $ appConfig config) (runWithConfig config) application

runWithConfig :: Config -> ReaderT Config IO a -> IO a
runWithConfig config reader = runReaderT reader config

application :: App ()
application = do
  get "/" $ do
    html $ renderText $ chromeHtml (Just htmlForSearchHeader) htmlForSearch
  get "/searches/:id" $ do
    searchId <- param "id"
    handleSearchGet searchId
  post "/searches" $ do
    from <- param "from"
    to <- param "to"
    date <- param "date"
    handleSearchPost from to date
  staticPath "static"

handleSearchGet :: Int -> Action ()
handleSearchGet searchId = do
  dbConfig <- lift $ asks databaseConfig
  let connectInfo = connectionInfo dbConfig
  dbSearch <- liftIO $ fetchSearchWithId connectInfo searchId
  case dbSearch of
    Just search -> do
      kiwiConfig <- lift $ asks kiwiConfig
      let searchEndpoint = kiwiEndpoint kiwiConfig
      currentFlightDetails <-
        liftIO $ cheapestFlightForSearch searchEndpoint search
      case currentFlightDetails of
        Just flight -> do
          flights <- liftIO $ fetchFlightsForSearch connectInfo searchId
          if length flights == 0
            then do
              liftIO $ void $ saveFlightResponse connectInfo searchId flight
              html $
                renderText $
                chromeHtml (Just htmlForFlightsHeader) $
                htmlForFlights flight flights
            else html $
                 renderText $
                 chromeHtml (Just htmlForFlightsHeader) $
                 htmlForFlights flight flights
        Nothing -> html $ renderText $ p_ "Couldn't find flight!"
    Nothing -> html $ renderText $ p_ "Couldn't find search!"

handleSearchPost :: String -> String -> String -> Action ()
handleSearchPost from to date = do
  dbConfig <- lift $ asks databaseConfig
  let connectInfo = connectionInfo dbConfig
  savedSearchId <- liftIO $ saveSearch connectInfo from to (dayFromDate date)
  redirect $ fromStrict $ (pack $ "/searches/" ++ (show savedSearchId))

dayFromDate :: String -> Day
dayFromDate date =
  let dateParts = splitOn "-" date
      intDateParts = map read dateParts :: [Int]
  in (fromGregorian
        (toInteger $ intDateParts !! 0)
        (intDateParts !! 1)
        (intDateParts !! 2))

staticPath :: String -> App ()
staticPath path =
  let routePattern = capture ("/" ++ path ++ "/:file")
  in get routePattern $ do
       fileName <- param "file"
       file $ "./" ++ path ++ "/" ++ fileName
