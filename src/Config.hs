{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Applicative
import Data.Maybe (fromJust)
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y
import Prelude

decodeConfig :: String -> IO (Config)
decodeConfig filePath = do
  fmap fromJust $ Y.decodeFile filePath

data Config = Config
  { databaseConfig :: DatabaseConfig
  , kiwiConfig :: KiwiConfig
  , appConfig :: AppConfig
  } deriving (Eq, Show)

data DatabaseConfig = DatabaseConfig
  { databaseHost :: String
  , databaseName :: String
  , databasePort :: Integer
  , databaseUsername :: String
  , databasePassword :: String
  } deriving (Eq, Show)

data KiwiConfig = KiwiConfig
  { kiwiEndpoint :: String
  } deriving (Eq, Show)

data AppConfig = AppConfig
  { outdatedIntervalSeconds :: Int
  , webServerPort :: Int
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$> v .: "database" <*> v .: "kiwi" <*> v .: "app"
  parseJSON _ = fail "Expected Object for Config value"

instance FromJSON DatabaseConfig where
  parseJSON (Y.Object v) =
    DatabaseConfig <$> v .: "host" <*> v .: "database" <*> v .: "port" <*>
    v .: "username" <*>
    v .: "password"
  parseJSON _ = fail "Expected Object for DatabaseConfig value"

instance FromJSON KiwiConfig where
  parseJSON (Y.Object v) = KiwiConfig <$> v .: "endpoint"
  parseJSON _ = fail "Expected Object for KiwiConfig value"

instance FromJSON AppConfig where
  parseJSON (Y.Object v) =
    AppConfig <$> v .: "outdatedIntervalSeconds" <*> v .: "webServerPort"
  parseJSON _ = fail "Expected Object for AppConfig value"
