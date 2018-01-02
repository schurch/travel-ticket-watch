{-# LANGUAGE OverloadedStrings #-}

module Main where

import Airports
import Config
import Control.Monad.Reader
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Text (pack, isInfixOf, toLower)
import Data.Text.Lazy (Text, fromStrict)
import Data.Time.Calendar (Day, fromGregorian)
import DataAccess
import Debug.Trace
import KiwiAPI
import Lucid (renderText, p_)
import Pages
import System.Environment
import System.Exit
import System.IO
import Types
import Web.Scotty.Trans
       (ScottyT, ActionT, get, scottyT, html, param, capture, file,
        redirect, post, json)

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
  airports <- loadAirportsFrom "airports.csv"
  scottyT
    (webServerPort $ appConfig config)
    (runWithConfig config)
    (application airports)

runWithConfig :: Config -> ReaderT Config IO a -> IO a
runWithConfig config reader = runReaderT reader config

application :: [Airport] -> App ()
application airports = do
  get "/" $ do
    html $
      renderText $
      chromeHtml (Just htmlForSearchHeader) (htmlForSearch airports)
  get "/searches/:id" $ do
    searchId <- param "id"
    -- TODO: Validate search Id
    handleSearchGet searchId
  post "/searches" $ do
    from <- param "from"
    to <- param "to"
    date <- param "date"
    -- TODO: Validate parameters
    handleSearchPost from to date
  get "/airports" $ do json $ airports
  get "/airports/:id" $ do
    airportIdParam <- param "id" :: Action Int
    json $ find (\a -> (airportId a) == airportIdParam) airports
  get "/airports/filter" $ do
    query <- param "query" :: Action String
    json $ filter (doesAirportMatchQuery query) airports
  staticPath "static"

doesAirportMatchQuery :: String -> Airport -> Bool
doesAirportMatchQuery query airport =
  let airportName' = toLower $ pack $ airportName airport
      iata = toLower $ pack $ airportIata airport
      query' = toLower $ pack query
  in isInfixOf query' iata || isInfixOf query' airportName'

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
  -- TODO: Try and lookup exiting search first
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
