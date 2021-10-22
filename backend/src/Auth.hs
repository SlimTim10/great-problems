module Auth
  ( authCheck
  , newSession
  , removeSession
  , getUser
  , Auth.Auth
  , Session
  , AuthResult(..)
  ) where

import Common.Lib.Prelude

import qualified Database.PostgreSQL.Simple as SQL

import qualified Common.Api.User as User
import qualified Common.Api.Auth as Auth
import qualified Database.Queries as Queries

type Session = Text

-- | The result of an authentication attempt.
-- Similar to Servant.Auth.Server.Internal.Types.AuthResult
data AuthResult val
  = Authenticated val
  | Unverified val
  | Indefinite
  deriving (Eq, Show, Read)

authCheck :: SQL.Connection -> Auth.Auth -> IO (AuthResult User.User)
authCheck conn auth = Queries.authenticate conn auth >>= \case
  Nothing -> return Indefinite
  Just user -> if not (User.verified user)
    then return $ Unverified user
    else return $ Authenticated user

newSession :: SQL.Connection -> User.User -> IO Session
newSession conn user = do
  -- Remove any existing (stale) sessions for this user
  Queries.removeSessionsByUserId conn (User.id user)
  Queries.newSession conn (User.id user)

removeSession :: SQL.Connection -> Session -> IO ()
removeSession conn session = Queries.removeSessionById conn session

getUser :: SQL.Connection -> Session -> IO (Maybe User.User)
getUser conn session = Queries.getUserFromSession conn session
