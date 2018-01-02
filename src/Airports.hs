{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Airports where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Vector (map, toList)
import Debug.Trace
import Types

loadAirportsFrom :: String -> IO [Airport]
loadAirportsFrom path = do
  csvData <- BL.readFile path
  case decode NoHeader csvData of
    Left _ -> return []
    Right value -> return $ toList $ Data.Vector.map tupleToAirport value

tupleToAirport
  :: (Int, String, String, String, String, String, Double, Double, Int, String, String, String, String, String)
  -> Airport
tupleToAirport (airportId, airportName, airportCity, airportCountry, airportIata, airportIcao, airportLatitude, airportLongitude, airportAltitude, airportTimezone, airportDst, airportTimezoneTz, airportType, airportDataSource) =
  Airport
    airportId
    airportName
    airportCity
    airportCountry
    airportIata
    airportIcao
    airportLatitude
    airportLongitude
    airportAltitude
    airportTimezone
    airportDst
    airportTimezoneTz
    airportType
    airportDataSource
