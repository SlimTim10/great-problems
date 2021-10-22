module Backend.Lib.Util where

import Common.Lib.Prelude

import qualified Text.Read
import qualified Crypto.PasswordStore
import qualified System.Random as Random
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Text.Encoding as TE
import qualified System.Environment as Env

whenM :: Monad m => Bool -> (a -> m a) -> a -> m a
whenM b f m = (if b then f else return) m

hashPassword :: Text -> IO Text
hashPassword password = cs <$> Crypto.PasswordStore.makePassword (cs $ password) 17

verifyPassword :: Text -> Text -> Bool
verifyPassword a b = Crypto.PasswordStore.verifyPassword (cs a) (cs b)

generateRandomText :: IO Text
generateRandomText = (Random.randomIO :: IO Word64)
  >>= return . TE.decodeUtf8 . B64URL.encode . cs . show

-- | Look up an environment variable, given a default to fall back to.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeEnv <- Env.lookupEnv env
  case maybeEnv of
    Nothing -> pure def
    Just str -> maybe (pure . read . show $ str) pure (Text.Read.readMaybe str)
