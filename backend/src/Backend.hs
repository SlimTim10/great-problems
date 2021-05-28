module Backend where

import Common.Route
import Obelisk.Backend

import qualified Database

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      conn <- Database.setup
      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
