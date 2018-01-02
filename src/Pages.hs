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
chromeHtml :: Maybe (Html ()) -> Html () -> Html ()
chromeHtml headerHtml bodyHtml =
  html_ $ do
    head_ $ do
      title_ "Ticket price watch"
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      fromMaybe ("" :: Html ()) headerHtml
    body_ bodyHtml

-- Search
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
          \$(this).find(\"input[name='date']\").remove();\
          \var date = $('#datepicker').datepicker('getDate');\
          \var formattedDate = $.datepicker.formatDate('yy-m-d', date);\
          \$(this).append('<input type=hidden name=date value='+formattedDate+' />');\
          \return true;\
        \});\
      \} );\
    \"

htmlForSearch :: [Airport] -> Html ()
htmlForSearch airports =
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

-- Flights
htmlForFlightsHeader :: Html ()
htmlForFlightsHeader = do
  link_ [rel_ "stylesheet", href_ "/static/tablesorter.styles.css"]
  script_ [src_ "https://code.jquery.com/jquery-1.12.4.js"] ("" :: String)
  script_ [src_ "/static/jquery.tablesorter.min.js"] ("" :: String)
  script_
    "\
      \$(document).ready(function() {\ 
        \$('#resultsTable').tablesorter();\
      \});\ 
    \"

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
