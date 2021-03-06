{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.DateTime (DateTime)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics

-- Airport
data Airport = Airport
  { airportId :: Int
  , airportName :: String
  , airportCity :: String
  , airportCountry :: String
  , airportIata :: String
  , airportIcao :: String
  , airportLatitude :: Double
  , airportLongitude :: Double
  , airportAltitude :: Int
  , airportTimezone :: String
  , airportDst :: String
  , airportTimezoneTz :: String
  , airportType :: String
  , airportDataSource :: String
  } deriving (Show, Eq, Generic)

-- instance ToJSON Airport
instance ToJSON Airport where
  toJSON a =
    object
      [ "id" .= airportId a
      , "name" .= airportName a
      , "city" .= airportCity a
      , "country" .= airportCountry a
      , "iata" .= airportIata a
      , "icao" .= airportIcao a
      , "latitude" .= airportLatitude a
      , "longitude" .= airportLongitude a
      , "altitude" .= airportAltitude a
      , "timezone" .= airportTimezone a
      , "dst" .= airportDst a
      , "timezoneTz" .= airportTimezoneTz a
      ]

instance FromJSON Airport

-- Kiwi API Types
data Results = Results
  { flights :: [FlightResponse]
  } deriving (Show, Eq)

instance FromJSON Results where
  parseJSON = withObject "Results" $ \v -> Results <$> v .: "data"

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

-- DB Types
data Flight = Flight
  { flightId :: Int
  , flightSearchId :: Int
  , flightUpdatedDate :: DateTime
  , price :: Double
  , depature :: DateTime
  , bookingLink :: Text
  , durationText :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Flight

instance FromJSON Flight

data Search = Search
  { searchId :: Int
  , searchUpdatedDate :: DateTime
  , flightFrom :: Text
  , flightTo :: Text
  , date :: Day
  , currency :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Search

instance FromJSON Search
