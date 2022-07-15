module Backend.Lib.Util where

import qualified Data.ByteString as BS
import qualified Text.Read
import qualified Crypto.PasswordStore
import qualified System.Random as Random
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Text.Encoding as TE
import qualified System.Environment as Env
import qualified Snap.Core as Snap
import qualified Data.Aeson as JSON

import Common.Lib.Prelude

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
lookupSetting env def = Env.lookupEnv env >>= \case
  Nothing -> pure def
  Just str -> maybe (pure . read . show $ str) pure (Text.Read.readMaybe str)

-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (Snap.MonadSnap m, JSON.ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  Snap.writeLBS . JSON.encode $ a

-- | Mark response as 'application/json'
jsonResponse :: Snap.MonadSnap m => m ()
jsonResponse = Snap.modifyResponse $
  Snap.setHeader "Content-Type" "application/json"

mkCookie
  :: Text -- ^ name
  -> Text -- ^ value
  -> Snap.Cookie
mkCookie name value = Snap.Cookie
  { Snap.cookieName = cs name :: BS.ByteString
  , Snap.cookieValue = cs value :: BS.ByteString
  , Snap.cookieExpires = Nothing
  , Snap.cookieDomain = Nothing
  , Snap.cookiePath = Just "/"
  , Snap.cookieSecure = True
  , Snap.cookieHttpOnly = False
  }

addCookie
  :: Snap.MonadSnap m
  => Text -- ^ cookie name
  -> Text -- ^ cookie value
  -> m ()
addCookie name value =
  Snap.modifyResponse
  $ Snap.addResponseCookie
  $ mkCookie name value

removeCookie
  :: Snap.MonadSnap m
  => Text -- ^ cookie name
  -> m ()
removeCookie name = Snap.expireCookie $ mkCookie name ""
