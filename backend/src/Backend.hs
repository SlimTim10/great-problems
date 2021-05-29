module Backend where

import Common.Route
import Obelisk.Backend

import qualified Database

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- Connect to the database (for production)
      -- conn <- Database.connect
      
      -- Database.setup clears and seeds the tables every time,
      -- so it should only be used during development/testing
      conn <- Database.setup
      
      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
