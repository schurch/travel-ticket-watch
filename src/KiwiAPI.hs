{-# LANGUAGE OverloadedStrings #-}

module KiwiAPI
  ( updatePriceForSearch
  , cheapestFlightForSearch
  ) where

import Control.Lens
import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Text (pack)
import Data.Time.Calendar (Day, toGregorian)
import DataAccess
import Database.MySQL.Simple
import Debug.Trace
import Network.Wreq
import Types

updatePriceForSearch :: ConnectInfo -> String -> Search -> IO ()
updatePriceForSearch connectInfo endpoint search = do
  lowestFlight <- cheapestFlightForSearch endpoint search
  case lowestFlight of
    Just flight -> do
      saveFlightResponse connectInfo (searchId search) flight
      updateSearchTimestamp connectInfo (searchId search)
      return ()
    Nothing -> return ()

cheapestFlightForSearch :: String -> Search -> IO (Maybe FlightResponse)
cheapestFlightForSearch endpoint search = do
  let query = (queryParametersFromSearch search)
  response <- getWith query endpoint >>= asJSON :: IO (Response Results)
  results <- return $ response ^. responseBody
  return $ findLowestFlight results

queryParametersFromSearch :: Search -> Options
queryParametersFromSearch search =
  let fromDate = Data.Text.pack $ dateToQueryDate (date search)
      toDate = Data.Text.pack $ dateToQueryDate (date search)
  in defaults & param "flyFrom" .~ [(flightFrom search)] & param "to" .~
     [(flightTo search)] &
     param "dateFrom" .~
     [fromDate] &
     param "dateTo" .~
     [toDate] &
     param "curr" .~
     [(currency search)] &
     param "partner" .~
     ["picky"]

dateToQueryDate :: Day -> String
dateToQueryDate date =
  let (year, month, day) = toGregorian date
  in show day ++ "/" ++ show month ++ "/" ++ show year

findLowestFlight :: Results -> Maybe FlightResponse
findLowestFlight results
  | (length (flights results)) == 0 = Nothing
  | otherwise = listToMaybe $ sortBy compareFlights (flights results)

compareFlights :: FlightResponse -> FlightResponse -> Ordering
compareFlights f1 f2
  | p1 < p2 = LT
  | p1 > p2 = GT
  | otherwise = EQ
  where
    p1 = flightResponsePrice f1
    p2 = flightResponsePrice f2
