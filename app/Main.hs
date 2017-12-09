{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Lens
import Control.Monad (forM_)
import Data.Aeson
import Data.DateTime (fromSeconds, DateTime)
import qualified Data.Int (Int64)
import Data.List (sortBy)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day, toGregorian)
import Database.MySQL.Simple
import Network.Wreq
import System.Environment

data Results = Results
  { count :: Int
  , flights :: [FlightResponse]
  } deriving (Show, Eq)

instance FromJSON Results where
  parseJSON =
    withObject "Results" $ \v -> Results <$> v .: "_results" <*> v .: "data"

data Flight = Flight
  { flightId :: Int
  , flightSearchId :: Int
  , price :: Double
  , bookingLink :: String
  , durationText :: String
  , departureUTC :: Integer
  } deriving (Show, Eq)

data FlightResponse = FlightResponse
  { flightResponsePrice :: Double
  , flightResponseBookingLink :: String
  , flightResponseDuration :: String
  , flightResponseDepartureUTC :: Integer
  } deriving (Show, Eq)

instance FromJSON FlightResponse where
  parseJSON =
    withObject "Flight" $ \v ->
      FlightResponse <$> v .: "price" <*> v .: "deep_link" <*>
      v .: "fly_duration" <*>
      v .: "dTimeUTC"

data Search = Search
  { searchId :: Int
  , flightFrom :: Text
  , flightTo :: Text
  , date :: Day
  , currency :: Text
  } deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  config <- decodeConfig $ Prelude.head args
  let connectInfo = connectionInfo $ (databaseConfig config)
  let outdatedInterval = (outdatedIntervalSeconds $ appConfig config)
  let endpoint = (kiwiEndpoint $ kiwiConfig config)
  searches <- fetchSearches connectInfo outdatedInterval
  forM_ searches (updatePriceForSearch connectInfo endpoint)

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
  }

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
