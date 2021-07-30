module Database where

import Prelude hiding (drop)

import qualified Database.PostgreSQL.Simple as SQL
import qualified System.Environment as Env
import qualified Configuration.Dotenv as Dotenv
import qualified Text.Read

import qualified Database.Schema
import qualified Database.Seeds
import Global

-- | Connect to the database using the variables in db.env, or default values.
connect :: IO SQL.Connection
connect = do
  let configPath = "config/backend/db.env"
  void $ Dotenv.loadFile $ Dotenv.defaultConfig
    { Dotenv.configPath = [configPath]
    }
  putStrLn "Loaded db.env"
    `Dotenv.onMissingFile` putStrLn (configPath ++ " not found")
  host <- lookupSetting "DB_HOST" "localhost"
  port <- lookupSetting "DB_PORT" 5432
  user <- lookupSetting "DB_USER" "root"
  password <- lookupSetting "DB_PASSWORD" "root"
  name <- lookupSetting "DB_NAME" "great_problems"
  SQL.connect SQL.defaultConnectInfo
    { SQL.connectHost = host
    , SQL.connectPort = port
    , SQL.connectUser = user
    , SQL.connectPassword = password
    , SQL.connectDatabase = name
    }

-- | Reset the schema and load the seeds.
reset :: IO SQL.Connection
reset = do
  conn <- connect
  Database.Schema.unload conn
  Database.Schema.load conn
  Database.Seeds.load conn
  return conn

-- | Look up an environment variable, given a default to fall back to.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeEnv <- Env.lookupEnv env
  case maybeEnv of
    Nothing -> pure def
    Just str -> maybe (pure . read . show $ str) pure (Text.Read.readMaybe str)
