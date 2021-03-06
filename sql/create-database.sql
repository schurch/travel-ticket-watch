CREATE DATABASE price_watch;

USE price_watch;

CREATE TABLE Searches (
   SearchID INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
   UpdatedDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
   FlightFrom VARCHAR(3) NOT NULL,
   FlightTo VARCHAR(3) NOT NULL,
   FlightDate DATE NOT NULL,
   Currency VARCHAR(3) NULL
);

CREATE TABLE Flights (
   FlightID INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
   SearchID INT NOT NULL,
   UpdatedDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
   Price DOUBLE NOT NULL,
   FlightDate DATETIME NOT NULL,
   BookingLink TEXT NOT NULL,
   FlightDurationText TEXT NOT NULL,
   FOREIGN KEY (SearchID) REFERENCES Searches(SearchID)
);
