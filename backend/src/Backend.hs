{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Backend where

import qualified Data.Text as Text
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Word as Word
import qualified Data.CaseInsensitive as CI
import qualified Snap.Core as Snap
import qualified Servant.Auth.Server as SAS
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
import Global

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
        Route.BackendRoute_Api :/ apiRoute -> do
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
                    Auth.verifyJWTUser jwtCfg >>= \case
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
                            secret <- IO.liftIO (Queries.newEmailVerification conn (User.id user))
                            IO.liftIO $ print ("-------------------------------- secret:" :: Text) -- DEBUG
                            IO.liftIO $ print secret -- DEBUG
                            -- send email with link http://localhost:8080/api/verify-email/secret
                            writeJSON OkResponse.OkResponse
            Route.Api_VerifyEmail :/ secret -> do
              verify <- IO.liftIO (Queries.verifyEmail conn secret)
              if verify
                then writeJSON OkResponse.OkResponse
                else writeJSON $ Error.mk "Invalid secret"
            Route.Api_SignIn :/ () -> do
              rawBody <- Snap.readRequestBody maxRequestBodySize
              case JSON.decode rawBody :: Maybe Auth.Auth of
                Nothing -> writeJSON $ Error.mk "Something went wrong"
                Just auth -> do
                  IO.liftIO (Auth.authCheck conn auth) >>= \case
                    SAS.Authenticated user -> do
                      Auth.makeJWTCookie jwtCfg user >>= \case
                        Just rawJWTCookie -> do
                          Auth.addCookie "user" (cs $ JSON.encode user)
                          Snap.modifyResponse $ Snap.setHeader "Set-Cookie" rawJWTCookie
                        Nothing -> writeJSON $ Error.mk "Something went wrong"
                    _ -> writeJSON $ Error.mk "Incorrect email or password. Please try again."
            Route.Api_SignOut :/ () -> Auth.signOut
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
