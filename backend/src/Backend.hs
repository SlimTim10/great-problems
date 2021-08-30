{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Backend where

import Obelisk.Route ( pattern (:/) )
import qualified Obelisk.Backend as Ob
import qualified Control.Monad.IO.Class as IO
import qualified Snap.Core as Snap
import qualified Servant.Auth.Server as SAS
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Word as Word
import qualified Data.ByteString as B

import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import qualified Database
import qualified Database.Queries as Queries
import qualified Common.Api.User as User
import qualified Common.Api.Auth as Auth
import Global

authCheck
  :: SQL.Connection
  -> Auth.Auth
  -> IO (SAS.AuthResult User.User)
authCheck conn auth = maybe SAS.Indefinite SAS.Authenticated <$> Queries.authenticate conn auth

maxRequestBodySize :: Word.Word64
maxRequestBodySize = 2048

backend :: Ob.Backend Route.BackendRoute Route.FrontendRoute
backend = Ob.Backend
  { Ob._backend_run = \serve -> do
      -- Connect to the database
      conn <- Database.connect

      jwk <- SAS.generateKey
      let jwtCfg = SAS.defaultJWTSettings jwk
      
      serve $ \case
        Route.BackendRoute_Missing :/ () -> return ()
        Route.BackendRoute_Api :/ apiRoute -> case apiRoute of
          Route.Api_Problems :/ subRoute -> case subRoute of
            (Nothing, query) -> writeJSON =<< IO.liftIO (Queries.getProblems conn Nothing query)
            (Just problemId, query) -> writeJSON =<< IO.liftIO (Queries.getProblemById conn problemId query)
          Route.Api_Topics :/ query -> do
            topics <- case fromMaybe Nothing (Map.lookup "parent" query) of
              Just "null" -> IO.liftIO (Queries.getRootTopics conn)
              Just x -> case readMaybe (cs x) :: Maybe Integer of
                Just parentId -> IO.liftIO (Queries.getTopicsByParentId conn parentId)
                Nothing -> IO.liftIO (Queries.getTopics conn)
              Nothing -> IO.liftIO (Queries.getTopics conn)
            writeJSON topics
          Route.Api_Users :/ subRoute -> case subRoute of
            Nothing -> writeJSON =<< IO.liftIO (Queries.getUsers conn)
            Just userId -> writeJSON =<< IO.liftIO (Queries.getUserById conn userId)
          Route.Api_TopicHierarchy :/ subRoute -> case subRoute of
            Nothing -> writeJSON $ Error.mk "Not yet implemented"
            Just topicId -> do
              IO.liftIO (Queries.getTopicById conn topicId) >>= \case
                Nothing -> writeJSON $ Error.mk "Topic not found"
                Just topic -> writeJSON =<< IO.liftIO (Queries.getTopicHierarchy conn topic)
          Route.Api_SignIn :/ () -> do
            rawBody <- Snap.readRequestBody maxRequestBodySize
            case JSON.decode rawBody :: Maybe Auth.Auth of
              Just auth -> do
                IO.liftIO (authCheck conn auth) >>= \case
                  SAS.Authenticated user -> do
                    mCookie <- IO.liftIO $ SAS.makeSessionCookieBS SAS.defaultCookieSettings jwtCfg user
                    IO.liftIO $ print mCookie
                    case mCookie of
                      Just cookie -> do
                        addResponseCookie "user" (cs $ JSON.encode user)
                        Snap.modifyResponse $ Snap.setHeader "Set-Cookie" cookie
                        writeJSON ()
                      Nothing -> writeJSON $ Error.mk "No cookie"
                  _ -> writeJSON $ Error.mk "Incorrect email or password. Please try again."
              Nothing -> writeJSON $ Error.mk "No auth in body"
  , Ob._backend_routeEncoder = Route.fullRouteEncoder
  }

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

addResponseCookie :: Snap.MonadSnap m => Text -> Text -> m ()
addResponseCookie name value = Snap.modifyResponse $ Snap.addResponseCookie $ Snap.Cookie
  (cs name :: B.ByteString)
  (cs value :: B.ByteString)
  Nothing
  Nothing
  (Just "/")
  False
  False
