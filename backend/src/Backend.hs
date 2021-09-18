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
import qualified Common.Api.Role as Role
import qualified Common.Api.Auth as Auth
import qualified Common.Api.NewProblem as NewProblem
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
      let jwtCfg :: SAS.JWTSettings = SAS.defaultJWTSettings jwk
      
      serve $ \case
        Route.BackendRoute_Missing :/ () -> return ()
        Route.BackendRoute_Api :/ apiRoute -> case apiRoute of
          Route.Api_Problems :/ subRoute -> case subRoute of
            (Nothing, query) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> writeJSON =<< IO.liftIO (Queries.getProblems conn Nothing query)
                Snap.POST -> do
                  verifyJWTUser jwtCfg >>= \case
                    Nothing -> writeJSON $ Error.mk "No access"
                    Just user -> do
                      rawBody <- Snap.readRequestBody maxRequestBodySize
                      case JSON.decode rawBody :: Maybe NewProblem.NewProblem of
                        Nothing -> writeJSON $ Error.mk "Invalid problem"
                        Just newProblem -> do
                          if
                            NewProblem.author_id newProblem /= User.id user
                            || not (User.role user == Role.Contributor || User.role user == Role.Moderator)
                            then writeJSON $ Error.mk "No access"
                            else writeJSON =<< IO.liftIO (Queries.addProblem conn newProblem)
                _ -> return () -- TODO: implement put, delete
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
                    makeJWTCookie jwtCfg user >>= \case
                      Just rawJWTCookie -> do
                        addCookie "user" (cs $ JSON.encode user)
                        Snap.modifyResponse $ Snap.setHeader "Set-Cookie" rawJWTCookie
                      Nothing -> writeJSON $ Error.mk "No cookie"
                  _ -> writeJSON $ Error.mk "Incorrect email or password. Please try again."
              Nothing -> writeJSON $ Error.mk "No auth in body"
          Route.Api_SignOut :/ () -> do
            removeCookie "user"
            removeCookie "JWT-Cookie"
  , Ob._backend_routeEncoder = Route.fullRouteEncoder
  }

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
