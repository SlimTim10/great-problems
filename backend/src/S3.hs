module S3 where

import qualified Configuration.Dotenv as Dotenv
import qualified System.Process as Proc

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
put env file destination = Proc.callProcess (s3cmdPath env) ["put", file, destination]

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
