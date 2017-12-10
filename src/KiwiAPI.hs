{-# LANGUAGE OverloadedStrings #-}

module KiwiAPI
  ( updatePriceForSearch
  ) where

import Control.Lens
import Data.List (sortBy)
import Data.Text (pack)
import Data.Time.Calendar (Day, toGregorian)
import DataAccess
import Database.MySQL.Simple
import Network.Wreq
import Types

updatePriceForSearch :: ConnectInfo -> String -> Search -> IO ()
updatePriceForSearch connectInfo endpoint search = do
  print search
  response <-
    getWith (queryParametersFromSearch search) endpoint >>= asJSON :: IO (Response Results)
  results <- return $ response ^. responseBody
  let lowestFlight = findLowestFlight results
  _ <- saveFlightResponse connectInfo (searchId search) lowestFlight
  _ <- updateSearchTimestamp connectInfo (searchId search)
  return ()

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

findLowestFlight :: Results -> FlightResponse
findLowestFlight results =
  Prelude.head $ sortBy compareFlights (flights results)

compareFlights :: FlightResponse -> FlightResponse -> Ordering
compareFlights f1 f2
  | p1 < p2 = LT
  | p1 > p2 = GT
  | otherwise = EQ
  where
    p1 = flightResponsePrice f1
    p2 = flightResponsePrice f2
