module Database where

import Prelude hiding (drop)
import Common.Lib.Prelude
import qualified Backend.Lib.Util as Util

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Migration as SQLM
import qualified Configuration.Dotenv as Dotenv

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

-- | Run all migrations that haven't yet run.
migrate :: IO (SQLM.MigrationResult String)
migrate = do
  let dir = "./backend/src/Database/Migrations"
  conn <- connect
  SQL.withTransaction conn $
    SQLM.runMigrations
    True
    conn
    [ SQLM.MigrationInitialization
    , SQLM.MigrationDirectory dir
    ]

-- | Reset the migration tracking (don't run any).
resetMigrations :: IO ()
resetMigrations = do
  conn <- connect
  void $ SQL.execute_ conn
    "DROP TABLE IF EXISTS schema_migrations CASCADE"
