{-# LANGUAGE OverloadedStrings #-}

module DataAccess
  ( connectionInfo
  , saveFlightResponse
  , updateSearchTimestamp
  , fetchSearches
  , fetchFlightsForSearch
  , saveSearch
  ) where

import Config
import Data.DateTime (DateTime, fromSeconds)
import qualified Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Calendar (Day, toGregorian)
import Database.MySQL.Simple
import Types

saveFlightResponse :: ConnectInfo
                   -> Int
                   -> FlightResponse
                   -> IO (Data.Int.Int64)
saveFlightResponse connectInfo searchID flightResponse = do
  conn <- connect connectInfo
  execute
    conn
    "INSERT INTO Flights (SearchID, Price, FlightDate, BookingLink, FlightDurationText) VALUES (?,?,?,?,?)"
    ( searchID
    , (flightResponsePrice flightResponse)
    , fromSeconds $ (flightResponseDepartureUTC flightResponse)
    , (flightResponseBookingLink flightResponse)
    , (flightResponseDuration flightResponse))

saveSearch :: ConnectInfo -> String -> String -> Day -> IO (Int)
saveSearch connectInfo from to date = do
  conn <- connect connectInfo
  execute conn "INSERT INTO Searches (FlightFrom, FlightTo, FlightDate, Currency) VALUES (?, ?, DATE(?), 'NZD');" (from, to, (dateToQueryDate date))
  [Only lastId] <- query_ conn "SELECT LAST_INSERT_ID();"
  return lastId

updateSearchTimestamp :: ConnectInfo -> Int -> IO (Data.Int.Int64)
updateSearchTimestamp connectInfo searchID = do
  conn <- connect connectInfo
  execute
    conn
    "UPDATE Searches SET UpdatedDate=NOW() WHERE SearchID = (?)"
    [searchID]

fetchSearches :: ConnectInfo -> Int -> IO [Search]
fetchSearches connectInfo outdatedIntervalSeconds = do
  conn <- connect connectInfo
  rows <-
    query
      conn
      "SELECT * FROM Searches WHERE UNIX_TIMESTAMP() - UNIX_TIMESTAMP(UpdatedDate) > (?)"
      [outdatedIntervalSeconds]
  return $ fmap rowToSearch rows

fetchFlightsForSearch :: ConnectInfo -> Int -> IO [Flight]
fetchFlightsForSearch connectInfo searchId = do
  conn <- connect connectInfo
  rows <- query conn "SELECT * FROM Flights WHERE SearchID = (?)" [searchId]
  return $ fmap rowToFlight rows

connectionInfo :: DatabaseConfig -> ConnectInfo
connectionInfo databaseConfig =
  defaultConnectInfo
  { connectHost = (databaseHost databaseConfig)
  , connectPort = fromInteger (databasePort databaseConfig)
  , connectUser = (databaseUsername databaseConfig)
  , connectPassword = (databasePassword databaseConfig)
  , connectDatabase = (databaseName databaseConfig)
  }

rowToSearch :: (Int, DateTime, Text, Text, Day, Text) -> Search
rowToSearch (searchId, updated, from, to, date, currency) =
  Search
  { searchId = searchId
  , flightFrom = from
  , flightTo = to
  , date = date
  , currency = currency
  , searchUpdatedDate = updated
  }

rowToFlight :: (Int, Int, DateTime, Double, DateTime, Text, Text) -> Flight
rowToFlight (flightId, searchId, updatedDate, price, departure, bookingLink, flightDurationText) =
  Flight
  { flightId = flightId
  , flightSearchId = searchId
  , flightUpdatedDate = updatedDate
  , price = price
  , depature = departure
  , bookingLink = bookingLink
  , durationText = flightDurationText
  }

dateToQueryDate :: Day -> String
dateToQueryDate date =
  let (year, month, day) = toGregorian date
  in show year ++ "-" ++ show month ++ "-" ++ show day
