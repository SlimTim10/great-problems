{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Route
import Obelisk.Backend
import Control.Monad.IO.Class (liftIO)

import qualified Database

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- Connect to the database (for production)
      -- conn <- Database.connect
      
      -- Database.setup clears and seeds the tables every time,
      -- so it should only be used during development/testing
      conn <- Database.setup

      serve $ \case
        BackendRoute_Missing :/ () -> return ()
        BackendRoute_Api :/ apiRoute -> case apiRoute of
          Api_Problems :/ () -> do
            liftIO $ putStrLn "api/problems"
            return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
