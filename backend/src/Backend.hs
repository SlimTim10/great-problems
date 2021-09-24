{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Backend where

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Word as Word
import qualified Data.CaseInsensitive as CI
import qualified Snap.Core as Snap
import qualified Obelisk.Backend as Ob
import Obelisk.Route ( pattern (:/) )

import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import qualified Database
import qualified Database.Queries as Queries
import qualified Common.Api.User as User
import qualified Common.Api.Register as Register
import qualified Common.Api.Role as Role
import qualified Common.Api.NewProblem as NewProblem
import qualified Common.Api.OkResponse as OkResponse
import qualified Auth
import qualified Email
import Global

maxRequestBodySize :: Word.Word64
maxRequestBodySize = 2048

backend :: Ob.Backend Route.BackendRoute Route.FrontendRoute
backend = Ob.Backend
  { Ob._backend_run = \serve -> do
      -- Connect to the database
      conn <- Database.connect

      serve $ \case
        Route.BackendRoute_Missing :/ () -> return ()
        Route.BackendRoute_Api :/ apiRoute -> do
          mSession :: Maybe Auth.Session <- maybe
            Nothing
            (pure . cs . Snap.cookieValue)
            <$> Snap.getCookie "sessionId"
          mUser :: Maybe User.User <- IO.liftIO $ maybe (pure Nothing) (Auth.getUser conn) mSession
          -- IO.liftIO $ print ("--------------------------------- user:" :: Text)
          -- user :: Maybe User.User <- verifyJWTUser jwtCfg
          -- IO.liftIO $ print ("--------------------------------- user:" :: Text)
          -- IO.liftIO $ print user
          -- when (isNothing user) signOut
          case apiRoute of
            Route.Api_Problems :/ subRoute -> case subRoute of
              (Nothing, query) -> do
                Snap.rqMethod <$> Snap.getRequest >>= \case
                  Snap.GET -> writeJSON =<< IO.liftIO (Queries.getProblems conn Nothing query)
                  Snap.POST -> do
                    case mUser of
                      Nothing -> writeJSON $ Error.mk "No access"
                      Just user -> do
                        rawBody <- Snap.readRequestBody maxRequestBodySize
                        case JSON.decode rawBody :: Maybe NewProblem.NewProblem of
                          Nothing -> writeJSON $ Error.mk "Invalid problem"
                          Just newProblem -> do
                            if NewProblem.author_id newProblem /= User.id user
                              || not (User.role user == Role.Contributor || User.role user == Role.Moderator)
                              then writeJSON $ Error.mk "No access"
                              else writeJSON =<< IO.liftIO (Queries.addProblem conn newProblem)
                  _ -> return () -- TODO: implement put, delete
              (Just problemId, query) -> writeJSON =<< IO.liftIO (Queries.getProblemById conn problemId query)
            Route.Api_Topics :/ query -> do
              topics <- case fromMaybe Nothing (Map.lookup "parent" query) of
                Just "null" -> IO.liftIO $ Queries.getRootTopics conn
                Just x -> case readMaybe (cs x) :: Maybe Integer of
                  Just parentId -> IO.liftIO $ Queries.getTopicsByParentId conn parentId
                  Nothing -> IO.liftIO $ Queries.getTopics conn
                Nothing -> IO.liftIO $ Queries.getTopics conn
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
            Route.Api_Register :/ () -> do
              rawBody <- Snap.readRequestBody maxRequestBodySize
              case JSON.decode rawBody :: Maybe Register.Register of
                Nothing -> writeJSON $ Error.mk "Something went wrong"
                Just register -> do
                  if any Text.null
                    [ CI.original $ Register.full_name register
                    , CI.original $ Register.email register
                    , Register.password register
                    ]
                    then writeJSON $ Error.mk "Fields must not be empty"
                    else do
                    IO.liftIO (Queries.getUserByEmail conn (Register.email register)) >>= \case
                      Just _ -> writeJSON $ Error.mk "Email already registered"
                      Nothing -> do
                        IO.liftIO (Queries.registerUser conn register) >>= \case
                          Nothing -> writeJSON $ Error.mk "Something went wrong"
                          Just user -> do
                            secret <- IO.liftIO $ Queries.newEmailVerification conn (User.id user)
                            Email.sendEmailVerification user secret
                            writeJSON OkResponse.OkResponse
            Route.Api_VerifyEmail :/ secret -> do
              verify <- IO.liftIO $ Queries.verifyEmail conn (cs secret)
              if verify
                then writeJSON OkResponse.OkResponse
                else writeJSON $ Error.mk "Invalid email verification code"
            Route.Api_SignIn :/ () -> do
              rawBody <- Snap.readRequestBody maxRequestBodySize
              case JSON.decode rawBody :: Maybe Auth.Auth of
                Nothing -> writeJSON $ Error.mk "Something went wrong"
                Just auth -> do
                  IO.liftIO (Auth.authCheck conn auth) >>= \case
                    Auth.Indefinite -> writeJSON $ Error.mk "Incorrect email or password. Please try again."
                    Auth.Unverified _ -> writeJSON $ Error.mk "Account not verified. Please check your email to complete the verification process."
                    Auth.Authenticated user -> do
                      session <- IO.liftIO $ Auth.newSession conn user
                      addCookie "sessionId" session
                      addCookie "user" (cs $ JSON.encode user)
                      writeJSON OkResponse.OkResponse
            Route.Api_SignOut :/ () -> do
              IO.liftIO $ mapM_ (Auth.removeSession conn) mSession
              removeCookie "sessionId"
              removeCookie "user"
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

mkCookie
  :: Text -- ^ name
  -> Text -- ^ value
  -> Snap.Cookie
mkCookie name value = Snap.Cookie
  (cs name :: BS.ByteString)
  (cs value :: BS.ByteString)
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
