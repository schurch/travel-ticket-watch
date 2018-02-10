{-# LANGUAGE OverloadedStrings #-}

module Pages
  ( chromeHtml
  , htmlForSearchHeader
  , htmlForSearch
  , htmlForFlightsHeader
  , htmlForFlights
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Lucid
import Types

-- Chrome
chromeHtml :: Maybe (Html ()) -> Html () -> Maybe String -> Html ()
chromeHtml headerHtml bodyHtml bodyId =
  html_ $ do
    head_ $ do
      title_ "Ticket price watch"
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      fromMaybe ("" :: Html ()) headerHtml
    body_ [id_ (fromMaybe (pack "") (pack <$> bodyId))] bodyHtml

-- Search
htmlForSearchHeader :: Html ()
htmlForSearchHeader = do
  link_
    [ rel_ "stylesheet"
    , href_ "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"
    ]
  script_ [src_ "https://code.jquery.com/jquery-1.12.4.js"] ("" :: String)
  script_ [src_ "https://code.jquery.com/ui/1.12.1/jquery-ui.js"] ("" :: String)
  script_ [src_ "/static/search.helpers.js"] ("" :: String)
  link_
    [ rel_ "stylesheet"
    , href_ "/static/styles.css"
    ]

htmlForSearch :: [Airport] -> Maybe String -> Html ()
htmlForSearch airports errorMessage  = do
  h1_ [id_ "header"] "Ticket Price Watch"
  p_ [] "Keep an eye on the cost of a flight."
  p_ [] "Check back to see how the cost changes over time."
  div_ [class_ "search-container"] $ do
    h3_ [] "Start searching"
    div_ [id_ "divider"] ""
    form_ [id_ "inputForm", action_ "/searches", method_ "post"] $ do
      (input_ [type_ "text", id_ "fromAirportTextInput", placeholder_ "From"])
      (input_ [type_ "text", id_ "toAirportTextInput", placeholder_ "To"])
      (input_ [type_ "text", id_ "datepicker", placeholder_ "On"])
      (input_ [type_ "submit", value_ "Submit"])
    case errorMessage of
      Nothing -> "" :: Html ()
      Just message -> div_ [style_ "margin-top: 10px"] $ toHtml message

airportSelection :: String -> Html ()
airportSelection postValue =
  select_ [name_ (pack postValue)] $ do
    (option_ [value_ "AKL"] "Auckland")
    (option_ [value_ "LHR"] "London Heathrow")

-- Flights
htmlForFlightsHeader :: Html ()
htmlForFlightsHeader = do
  link_ [rel_ "stylesheet", href_ "/static/tablesorter.styles.css"]
  script_ [src_ "https://code.jquery.com/jquery-1.12.4.js"] ("" :: String)
  script_ [src_ "/static/jquery.tablesorter.min.js"] ("" :: String)
  script_ [src_ "/static/flight.helpers.js"] ("" :: String)
  link_
    [ rel_ "stylesheet"
    , href_ "/static/styles.css"
    ]


htmlForFlights :: FlightResponse -> [Flight] -> Html ()
htmlForFlights currentFlightDetails flights = do
  let bookingLink = pack (flightResponseBookingLink currentFlightDetails)
  p_ $
    toHtml $
    "Current price: $" ++ show (flightResponsePrice currentFlightDetails)
    -- TODO: Show flight date
  p_ $ a_ [href_ bookingLink] "Book now"
  if length flights > 0
    then p_ $
         table_ [id_ "resultsTable", class_ "tablesorter"] $ do
           thead_ $
             tr_ $ do
               th_ "Date"
               th_ "Price"
               th_ "Duration"
               th_ ""
           tbody_ $ do mapM_ flightToHtml flights
    else return ()

flightToHtml :: Flight -> Html ()
flightToHtml flight =
  tr_ $ do
    td_ $ toHtml $ show (flightUpdatedDate flight)
    td_ $ toHtml $ show (price flight)
    td_ $ toHtml (durationText flight)
    td_ $ a_ [href_ (bookingLink flight)] "Book"
