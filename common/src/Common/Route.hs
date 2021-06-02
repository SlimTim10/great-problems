{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity (Identity)
import Obelisk.Route ( pattern (:/) )
import qualified Obelisk.Route as O
import qualified Obelisk.Route.TH as O

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute (O.R Api)

data Api :: * -> * where
  Api_Problems :: Api ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_New :: FrontendRoute ()

fullRouteEncoder
  :: O.Encoder (Either Text) Identity (O.R (O.FullRoute BackendRoute FrontendRoute)) O.PageName
fullRouteEncoder = O.mkFullRouteEncoder
  (O.FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> O.PathSegment "missing" $ O.unitEncoder mempty
      BackendRoute_Api -> O.PathSegment "api" $ O.pathComponentEncoder $ \case
        Api_Problems -> O.PathSegment "problems" $ O.unitEncoder mempty)
  (\case
      FrontendRoute_Main -> O.PathEnd $ O.unitEncoder mempty
      FrontendRoute_New -> O.PathSegment "new" $ O.unitEncoder mempty)

concat <$> mapM O.deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

O.deriveRouteComponent ''Api
