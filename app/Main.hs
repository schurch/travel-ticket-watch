{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.List.Split
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text.Lazy (fromStrict)
import Data.Time.Calendar (Day, fromGregorian)
import DataAccess
import Database.MySQL.Simple (ConnectInfo)
import KiwiAPI
import Lucid
import System.Environment
import System.Exit
import System.IO
import Types
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
  runScotty config

runScotty :: Config -> IO ()
runScotty config = do
  scotty (webServerPort $ appConfig config) $ do
    get "/" $ do
      html $ renderText $ chromeHtml (Just htmlForSearchHeader) htmlForSearch
    get "/searches/:id" $ do
      let connectInfo = connectionInfo $ (databaseConfig config)
      let endpoint = (kiwiEndpoint $ kiwiConfig config)
      searchId <- param "id"
      handleSearchGet connectInfo endpoint searchId
    post "/searches" $ do
      let connectInfo = connectionInfo $ (databaseConfig config)
      from <- param "from"
      to <- param "to"
      date <- param "date"
      handleSearchPost connectInfo from to date

handleSearchGet :: ConnectInfo -> String -> Int -> ActionM ()
handleSearchGet connectInfo searchEndpoint searchId = do
  dbSearch <- liftIO $ fetchSearchWithId connectInfo searchId
  case dbSearch of
    Just search -> do
      currentFlightDetails <-
        liftIO $ cheapestFlightForSearch searchEndpoint search
      case currentFlightDetails of
        Just flight -> do
          flights <- liftIO $ fetchFlightsForSearch connectInfo searchId
          -- liftIO $ saveFlightResponse connectInfo searchId flight
          html $ renderText $ chromeHtml Nothing $ htmlForFlights flight flights
        Nothing -> html $ renderText $ p_ "Couldn't find flight!"
    Nothing -> html $ renderText $ p_ "Couldn't find search!"

handleSearchPost :: ConnectInfo -> String -> String -> String -> ActionM ()
handleSearchPost connectInfo from to date = do
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

staticPath :: String -> ScottyM ()
staticPath path =
  let routePattern = capture ("/" ++ path ++ "/:file")
  in get routePattern $ do
       fileName <- param "file"
       file $ "./" ++ path ++ "/" ++ fileName

chromeHtml :: Maybe (Html ()) -> Html () -> Html ()
chromeHtml headerHtml bodyHtml =
  html_ $ do
    head_ $ do
      title_ "Ticket price watch"
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      fromMaybe ("" :: Html ()) headerHtml
    body_ bodyHtml

htmlForSearchHeader :: Html ()
htmlForSearchHeader = do
  link_
    [ rel_ "stylesheet"
    , href_ "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"
    ]
  script_ [src_ "https://code.jquery.com/jquery-1.12.4.js"] ("" :: String)
  script_ [src_ "https://code.jquery.com/ui/1.12.1/jquery-ui.js"] ("" :: String)
  script_
    "\
        \$( function() {\
          \$('#datepicker').datepicker();\
          \$('#datepicker').datepicker('option', 'dateFormat', 'DD, d MM, yy');\
          \$('#inputForm').submit(function() {\
              \var date = $('#datepicker').datepicker('getDate');\
              \var formattedDate = $.datepicker.formatDate('yy-m-d', date);\
              \$(this).append('<input type=hidden name=date value='+formattedDate+' />');\
              \return true;\
          \});\
        \} );\
      \"

htmlForSearch :: Html ()
htmlForSearch =
  form_ [id_ "inputForm", action_ "/searches", method_ "post"] $ do
    (airportSelection "from")
    " to "
    (airportSelection "to")
    " on "
    (input_ [type_ "text", id_ "datepicker"])
    (input_ [type_ "submit", value_ "Submit"])

airportSelection :: String -> Html ()
airportSelection postValue =
  select_ [name_ (pack postValue)] $ do
    (option_ [value_ "AKL"] "Auckland")
    (option_ [value_ "LHR"] "London Heathrow")

htmlForFlights :: FlightResponse -> [Flight] -> Html ()
htmlForFlights currentFlightDetails flights = do
  p_ $
    toHtml $ "Current price" ++ show (flightResponsePrice currentFlightDetails)
  p_ $
    toHtml $ "Current link" ++ (flightResponseBookingLink currentFlightDetails)
