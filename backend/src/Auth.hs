module Auth
  ( authCheck
  , signOut
  , verifyJWTUser
  , makeJWTCookie
  , addCookie
  , Auth.Auth
  ) where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Servant.Auth.Server as SAS
import qualified Snap.Core as Snap
import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString as B

import qualified Common.Api.User as User
import qualified Common.Api.Auth as Auth
import qualified Database.Queries as Queries
import Global

authCheck
  :: SQL.Connection
  -> Auth.Auth
  -> IO (SAS.AuthResult User.User)
authCheck conn auth = maybe SAS.Indefinite SAS.Authenticated <$> Queries.authenticate conn auth

signOut :: Snap.Snap ()
signOut = do
  removeCookie "user"
  removeCookie "JWT-Cookie"

verifyJWTUser :: SAS.JWTSettings -> Snap.Snap (Maybe User.User)
verifyJWTUser jwtCfg = do
  jwt :: Text <- Snap.readCookie "JWT-Cookie"
  IO.liftIO $ SAS.verifyJWT jwtCfg (cs jwt)

makeJWTCookie
  :: ( IO.MonadIO m
     , SAS.ToJWT v
     )
  => SAS.JWTSettings
  -> v
  -> m (Maybe B.ByteString)
makeJWTCookie jwtCfg user = IO.liftIO $
  SAS.makeSessionCookieBS SAS.defaultCookieSettings jwtCfg user

mkCookie
  :: Text -- ^ name
  -> Text -- ^ value
  -> Snap.Cookie
mkCookie name value = Snap.Cookie
  (cs name :: B.ByteString)
  (cs value :: B.ByteString)
  Nothing
  Nothing
  (Just "/")
  False
  False

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

-- generateSessionId = do
--   x <- randomIO :: IO Int64
--   let sessionId = _
--   -- check for sessionId in db
