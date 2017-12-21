{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.DateTime (DateTime)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics
import Text.Mustache

-- Kiwi API Types
data Results = Results
  { count :: Int
  , flights :: [FlightResponse]
  } deriving (Show, Eq)

instance FromJSON Results where
  parseJSON =
    withObject "Results" $ \v -> Results <$> v .: "_results" <*> v .: "data"

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

instance ToMustache Flight where
  toMustache flight =
    Text.Mustache.object
      [ "flightId" ~> flightId flight
      , "price" ~> price flight
      , "bookingLink" ~> bookingLink flight
      ]

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
