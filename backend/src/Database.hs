module Database where

import Prelude hiding (drop)
import Common.Lib.Prelude
import qualified Backend.Lib.Util as Util

import qualified Database.PostgreSQL.Simple as SQL
import qualified Configuration.Dotenv as Dotenv

import qualified Database.Schema
import qualified Database.Seeds
import qualified Database.Types.Role as DbRole
import qualified Database.Types.User as DbUser

-- | Connect to the database using the variables in db.env, or default values.
connect :: IO SQL.Connection
connect = do
  let configPath = "config/backend/db.env"
  void $ Dotenv.loadFile $ Dotenv.defaultConfig
    { Dotenv.configPath = [configPath]
    }
  putStrLn "Loaded db.env"
    `Dotenv.onMissingFile` putStrLn (configPath ++ " not found")
  host <- Util.lookupSetting "DB_HOST" "localhost"
  port <- Util.lookupSetting "DB_PORT" 5432
  user <- Util.lookupSetting "DB_USER" "root"
  password <- Util.lookupSetting "DB_PASSWORD" "root"
  name <- Util.lookupSetting "DB_NAME" "great_problems"
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
  putStrLn "Connecting to database..."
  conn <- connect
  putStrLn "Dropping tables..."
  Database.Schema.unload conn
  putStrLn "Creating tables..."
  Database.Schema.load conn
  putStrLn "Inserting seeds..."
  Database.Seeds.load conn
  putStrLn "Complete!"
  return conn

-- | Setup the schema and load the seeds.
setup :: IO SQL.Connection
setup = do
  putStrLn "Connecting to database..."
  conn <- connect
  putStrLn "Creating tables..."
  Database.Schema.load conn
  putStrLn "Inserting seeds..."
  Database.Seeds.load conn
  putStrLn "Complete!"
  return conn

-- | Add a user (verified) to the database. Intended for repl use only.
addUser
  :: String -- ^ Full name
  -> String -- ^ Email
  -> String -- ^ Password
  -> String -- ^ Role name (e.g., "Contributor")
  -> IO ()
addUser name email password roleName = do
  conn <- connect
  hPassword <- Util.hashPassword (cs password)
  role :: DbRole.Role <- head <$> SQL.query conn "SELECT * FROM roles WHERE name = ?" (SQL.Only roleName)
  mUser :: Maybe DbUser.User <- headMay
    <$> SQL.query conn
    "INSERT INTO users(full_name, email, password, role_id, verified) VALUES (?,?,?,?,TRUE) returning *"
    ( name
    , email
    , hPassword
    , DbRole.id role
    )
  case mUser of
    Nothing -> putStrLn "Failed"
    Just user -> do
      putStrLn "Added user:"
      print user
