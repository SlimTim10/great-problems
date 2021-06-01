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
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute (R Api)

data Api :: * -> * where
  Api_Problems :: Api ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_New :: FrontendRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api -> PathSegment "api" $ pathComponentEncoder $ \case
        Api_Problems -> PathSegment "problems" $ unitEncoder mempty)
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_New -> PathSegment "new" $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

deriveRouteComponent ''Api
