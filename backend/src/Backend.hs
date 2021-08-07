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
          Route.Api_Problems :/ () -> do
            writeJSON =<< IO.liftIO (Queries.getProblemTiles conn)
          Route.Api_Topics :/ apiRoute' -> case apiRoute' of
            Nothing -> do
              writeJSON =<< IO.liftIO (Queries.getTopics conn)
            Just (Route.Api_RootTopics :/ ()) -> do
              writeJSON =<< IO.liftIO (Queries.getRootTopics conn)
          Route.Api_Users :/ apiRoute' -> case apiRoute' of
            Nothing -> do
              writeJSON =<< IO.liftIO (Queries.getUsers conn)
            Just userId -> do
              writeJSON =<< IO.liftIO (Queries.getUserById conn userId)
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
