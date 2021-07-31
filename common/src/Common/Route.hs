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

import Control.Category ((>>>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
import Obelisk.Route ( pattern (:/) )
import qualified Obelisk.Route as Ob
import qualified Obelisk.Route.TH as Ob

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute (Ob.R Api)

data Api :: * -> * where
  Api_Problems :: Api ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_New :: FrontendRoute ()
  FrontendRoute_ViewProblem :: FrontendRoute Integer

idPathSegmentEncoder
  :: Ob.Encoder (Either Text) (Either Text) Integer Ob.PageName
idPathSegmentEncoder = idEncoder >>> Ob.singlePathSegmentEncoder
  where
    idEncoder = Ob.unsafeMkEncoder Ob.EncoderImpl
      { Ob._encoderImpl_encode = T.pack . show
      , Ob._encoderImpl_decode = Ob.tryDecode Ob.unsafeTshowEncoder
      }

fullRouteEncoder
  :: Ob.Encoder (Either Text) Identity (Ob.R (Ob.FullRoute BackendRoute FrontendRoute)) Ob.PageName
fullRouteEncoder = Ob.mkFullRouteEncoder
  (Ob.FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> Ob.PathSegment "missing" $ Ob.unitEncoder mempty
      BackendRoute_Api -> Ob.PathSegment "api" $ Ob.pathComponentEncoder $ \case
        Api_Problems -> Ob.PathSegment "problems" $ Ob.unitEncoder mempty)
  (\case
      FrontendRoute_Main -> Ob.PathEnd $ Ob.unitEncoder mempty
      FrontendRoute_New -> Ob.PathSegment "new" $ Ob.unitEncoder mempty
      FrontendRoute_ViewProblem -> Ob.PathSegment "problems" idPathSegmentEncoder
  )

concat <$> mapM Ob.deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

Ob.deriveRouteComponent ''Api
