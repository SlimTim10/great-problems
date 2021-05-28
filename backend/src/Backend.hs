module Backend where

import Common.Route
import Obelisk.Backend

import qualified Database as Database

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      conn <- Database.connect
      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
