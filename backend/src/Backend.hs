{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Backend where

import Obelisk.Route ( pattern (:/) )
import qualified Common.Route as Route
import qualified Obelisk.Backend as O
import qualified Control.Monad.IO.Class as IO
import qualified Snap.Core as Snap
import qualified Data.Aeson as JSON

import qualified Database
import qualified Database.Queries as Queries

backend :: O.Backend Route.BackendRoute Route.FrontendRoute
backend = O.Backend
  { O._backend_run = \serve -> do
      -- Connect to the database (for production)
      -- conn <- Database.connect
      
      -- Database.setup clears and seeds the tables every time,
      -- so it should only be used during development/testing
      conn <- Database.setup

      serve $ \case
        Route.BackendRoute_Missing :/ () -> return ()
        Route.BackendRoute_Api :/ apiRoute -> case apiRoute of
          Route.Api_Problems :/ () -> do
            writeJSON =<< IO.liftIO (Queries.getProblems conn)
  , O._backend_routeEncoder = Route.fullRouteEncoder
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
