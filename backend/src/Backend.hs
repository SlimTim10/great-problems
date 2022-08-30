{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import qualified Control.Monad.IO.Class as IO
import qualified Snap.Core as Snap
import qualified Obelisk.Backend as Ob

import Common.Lib.Prelude
import qualified Common.Route as Route
import qualified Database
import qualified Common.Api.Error as Error
import qualified Common.Api.User as User
import qualified Auth
import qualified Route.Handlers as Handlers
import qualified Backend.Lib.Util as Util

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
          when (isNothing mUser) $ do
            Util.removeCookie "sessionId"
            Util.removeCookie "user"
            
          case apiRoute of
            
            Route.Api_Problems :/ (Nothing, query) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getProblems conn mUser query
                Snap.POST -> Handlers.saveProblem conn mUser
                _ -> return ()
                
            Route.Api_Problems :/ (Just problemId, _) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getProblem conn problemId
                Snap.DELETE -> Handlers.deleteProblem conn mUser problemId
                _ -> return ()

            Route.Api_ProblemSets :/ (Nothing, query) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getProblemSets conn query
                Snap.POST -> Handlers.saveProblemSet conn mUser
                _ -> return ()
                
            Route.Api_ProblemSets :/ (Just problemSetId, _) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getProblemSet conn problemSetId
                Snap.POST -> Handlers.addProblemToSet conn mUser problemSetId
                Snap.PUT -> Handlers.editProblemSet conn mUser problemSetId
                Snap.DELETE -> Handlers.deleteProblemSet conn mUser problemSetId
                _ -> return ()

            Route.Api_Topics :/ (Nothing, query) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getTopics conn query
                Snap.POST -> Handlers.createTopic conn mUser
                _ -> return ()

            Route.Api_Topics :/ (Just topicId, _) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getTopic conn topicId
                Snap.POST -> Handlers.updateTopic conn mUser topicId
                Snap.DELETE -> Handlers.deleteTopic conn mUser topicId
                _ -> return ()
              
            Route.Api_Users :/ Nothing -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getUsers conn
                _ -> return ()
                
            Route.Api_Users :/ (Just userId) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getUser conn mUser userId
                Snap.POST -> Handlers.updateUser conn mUser userId
                _ -> return ()
                
            Route.Api_Roles :/ () -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getRoles conn
                _ -> return ()

            Route.Api_TopicHierarchy :/ Nothing -> do
              Util.writeJSON $ Error.mk "Not yet implemented"
              
            Route.Api_TopicHierarchy :/ Just topicId -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getTopicHierarchy conn topicId
                _ -> return ()

            Route.Api_Register :/ () -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.POST -> Handlers.registerUser conn
                _ -> return ()

            Route.Api_VerifyEmail :/ secret -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.verifyEmail conn secret
                _ -> return ()
    
            Route.Api_ChangePassword :/ () -> Snap.rqMethod <$> Snap.getRequest >>= \case
              Snap.POST -> Handlers.changePassword conn mUser
              _ -> return ()
                
            Route.Api_SignIn :/ () -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.POST -> Handlers.signIn conn
                _ -> return ()

            Route.Api_SignOut :/ () -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.POST -> Handlers.signOut conn mSession
                _ -> return ()

            Route.Api_DuplicateProblem :/ problemId -> do
              -- Current user must have the right role and problem must be published
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.POST -> Handlers.duplicateProblem conn mUser problemId
                _ -> return ()

            Route.Api_ResetPassword :/ () -> Snap.rqMethod <$> Snap.getRequest >>= \case
              Snap.POST -> Handlers.resetPassword conn
              _ -> return ()

            -- Hide DB email information from the client by always sending back OkResponse
            Route.Api_ResendEmail :/ () -> Snap.rqMethod <$> Snap.getRequest >>= \case
              Snap.POST -> Handlers.resendEmail conn
              _ -> return ()
              
            Route.Api_Compile :/ Nothing -> Snap.rqMethod <$> Snap.getRequest >>= \case
              Snap.POST -> Handlers.compileProblem >>= \case
                Left e -> Util.writeJSON e
                Right html -> Util.writeJSON html
              _ -> return ()
              
            Route.Api_Compile :/ Just problemId -> Snap.rqMethod <$> Snap.getRequest >>= \case
              Snap.POST -> Handlers.compileProblemById conn problemId
              _ -> return ()
              
            Route.Api_Figures :/ figureId -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getFigure conn figureId
                _ -> return ()

            Route.Api_MetaSettings :/ Nothing -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getMetaSettings conn mUser
                Snap.POST -> Handlers.setMetaSetting conn mUser
                _ -> return ()

            Route.Api_MetaSettings :/ (Just setting) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> Handlers.getMetaSetting conn setting
                _ -> return ()

  , Ob._backend_routeEncoder = Route.fullRouteEncoder
  }
