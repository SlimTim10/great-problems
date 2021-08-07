{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Backend where

import Obelisk.Route ( pattern (:/) )
import qualified Obelisk.Backend as Ob
import qualified Control.Monad.IO.Class as IO
import qualified Snap.Core as Snap
import qualified Data.Aeson as JSON

import qualified Common.Route as Route
import qualified Database
import qualified Database.Queries as Queries

backend :: Ob.Backend Route.BackendRoute Route.FrontendRoute
backend = Ob.Backend
  { Ob._backend_run = \serve -> do
      -- Connect to the database
      conn <- Database.connect
      
      serve $ \case
        Route.BackendRoute_Missing :/ () -> return ()
        Route.BackendRoute_Api :/ apiRoute -> case apiRoute of
          Route.Api_Problems :/ subRoute -> case subRoute of
            Nothing -> writeJSON =<< IO.liftIO (Queries.getProblemCards conn)
            -- TODO: get single extended problem
            -- Just problemId -> writeJSON =<< IO.liftIO (Queries.getExtendedProblemById conn problemId)
            Just _ -> writeJSON =<< IO.liftIO (Queries.getProblemCards conn)
          Route.Api_Topics :/ subRoute -> case subRoute of
            Nothing -> writeJSON =<< IO.liftIO (Queries.getTopics conn)
            Just (Route.Api_RootTopics :/ ()) -> writeJSON =<< IO.liftIO (Queries.getRootTopics conn)
          Route.Api_Users :/ subRoute -> case subRoute of
            Nothing -> writeJSON =<< IO.liftIO (Queries.getUsers conn)
            Just userId -> writeJSON =<< IO.liftIO (Queries.getUserById conn userId)
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
