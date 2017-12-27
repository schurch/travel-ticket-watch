{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Data.Text (pack)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Text.Lazy (fromStrict)
import Data.Time.Calendar (Day, fromGregorian)
import DataAccess
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
  runScotty config

runScotty :: Config -> IO ()
runScotty config = do
  scotty 3000 $ do
    get "/" $ do
      html $ renderText htmlForSearch
    get "/flights/:id" $ do
      let connectInfo = connectionInfo $ (databaseConfig config)
      searchId <- param "id"
      flights <- liftIO $ fetchFlightsForSearch connectInfo searchId
      html $ renderText (htmlForFlights flights)
    post "/searches" $ do
      let connectInfo = connectionInfo $ (databaseConfig config)
      from <- param "from"
      to <- param "to"
      savedSearchId <- liftIO $ saveSearch connectInfo from to (fromGregorian 2018 5 1)
      redirect $ fromStrict $ (pack $ "/flights/" ++ (show savedSearchId))

staticPath :: String -> ScottyM ()
staticPath path =
  let routePattern = capture ("/" ++ path ++ "/:file")
  in get routePattern $ do
       fileName <- param "file"
       file $ "./" ++ path ++ "/" ++ fileName

htmlForSearch :: Html ()
htmlForSearch = (form_ [action_ "/searches", method_ "post"] (do
                  (airportSelection "From" "from")
                  "to"
                  (airportSelection "To" "to")
                  (input_ [type_ "submit", value_ "submit"])))

airportSelection :: String -> String -> Html ()
airportSelection title postValue = (select_ [name_ (pack postValue)] (do
                   (option_ [value_ "AKL"] "Auckland")
                   (option_ [value_ "LHR"] "London Heathrow")))

htmlForFlights :: [Flight] -> Html ()
htmlForFlights flights = (p_ "hello")
