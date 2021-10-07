module S3 where

import qualified Configuration.Dotenv as Dotenv
import qualified System.Process as Sys

import qualified Util
import Global

data Env = Env
  { s3cmdPath :: String
  , bucket :: String
  }

-- | Get the variables in s3.env, or default values.
setup :: IO Env
setup = do
  let configPath = "config/backend/s3.env"
  void $ Dotenv.loadFile $ Dotenv.defaultConfig
    { Dotenv.configPath = [configPath]
    }
  putStrLn "Loaded s3.env"
    `Dotenv.onMissingFile` putStrLn (configPath ++ " not found")
  s3cmdPath' <- Util.lookupSetting "S3CMD_PATH" "/usr/bin/s3cmd"
  bucket' <- Util.lookupSetting "S3_BUCKET" "greatproblems"
  return $ Env s3cmdPath' bucket'

-- | Put file into bucket.
put
  :: Env
  -> String -- ^ Path to file
  -> String -- ^ Destination (s3://BUCKET[/PREFIX])
  -> IO ()
put env file destination = Sys.callProcess (s3cmdPath env) ["put", file, destination]

-- | Put problem figure into bucket.
putFigure
  :: Env
  -> String -- ^ Path to figure
  -> Integer -- ^ Problem ID that this figure belongs to
  -> String -- ^ Figure name
  -> IO ()
putFigure env file problemId name = do
  let destination = cs $ "s3://"
        <> (bucket env)
        <> "/figures/"
        <> show problemId <> "/"
        <> name
  put env file destination

-- | Synchronize a directory tree to S3 (checks files freshness using size and md5 checksum, unless overridden by options (see https://s3tools.org/usage).
sync
  :: Env
  -> String -- ^ Local or remote directory
  -> String -- ^ Local or remote directory
  -> IO ()
sync env a b = Sys.callProcess (s3cmdPath env) ["sync", a, b]

-- | Put problem figures from directory into bucket.
putFigureDirectory
  :: Env
  -> String -- ^ Path to directory
  -> Integer -- ^ Problem ID that these figures belong to
  -> IO ()
putFigureDirectory env dir problemId = do
  let destination = cs $ "s3://"
        <> (bucket env)
        <> "/figures/"
        <> show problemId <> "/"
  sync env dir destination
