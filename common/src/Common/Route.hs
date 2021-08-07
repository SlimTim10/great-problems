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

import Prelude hiding (id, (.))

import Control.Category ((>>>), (.))
import Control.Categorical.Bifunctor (bimap)
import Data.Text (Text)
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
  Api_Topics :: Api (Maybe (Ob.R Api_Topics))
  Api_Users :: Api (Maybe Integer)

data Api_Topics :: * -> * where
  Api_RootTopics :: Api_Topics ()

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Explore :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_SignIn :: FrontendRoute ()
  FrontendRoute_New :: FrontendRoute ()
  FrontendRoute_ViewProblem :: FrontendRoute Integer
  FrontendRoute_Topics :: FrontendRoute (Integer, Maybe (Ob.R TopicsRoute))

data TopicsRoute :: * -> * where
  TopicsRoute_Problems :: TopicsRoute ()
  TopicsRoute_ProblemSets :: TopicsRoute ()

idPathSegmentEncoder
  :: Ob.Encoder (Either Text) (Either Text) Integer Ob.PageName
idPathSegmentEncoder = Ob.unsafeTshowEncoder >>> Ob.singlePathSegmentEncoder

fullRouteEncoder
  :: Ob.Encoder (Either Text) Identity (Ob.R (Ob.FullRoute BackendRoute FrontendRoute)) Ob.PageName
fullRouteEncoder = Ob.mkFullRouteEncoder
  (Ob.FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> Ob.PathSegment "missing" $ Ob.unitEncoder mempty
      BackendRoute_Api -> Ob.PathSegment "api" $ Ob.pathComponentEncoder $ \case
        Api_Problems -> Ob.PathSegment "problems" $ Ob.unitEncoder mempty
        Api_Topics -> Ob.PathSegment "topics" $
          Ob.maybeEncoder (Ob.unitEncoder mempty) $ Ob.pathComponentEncoder $ \case
          Api_RootTopics -> Ob.PathSegment "roots" $ Ob.unitEncoder mempty
        Api_Users -> Ob.PathSegment "users" $
          Ob.maybeEncoder (Ob.unitEncoder mempty) $ idPathSegmentEncoder
  )
  (\case
      FrontendRoute_Home -> Ob.PathEnd $ Ob.unitEncoder mempty
      FrontendRoute_Explore -> Ob.PathSegment "explore" $ Ob.unitEncoder mempty
      FrontendRoute_Register -> Ob.PathSegment "register" $ Ob.unitEncoder mempty
      FrontendRoute_SignIn -> Ob.PathSegment "sign-in" $ Ob.unitEncoder mempty
      FrontendRoute_New -> Ob.PathSegment "new" $ Ob.unitEncoder mempty
      FrontendRoute_ViewProblem -> Ob.PathSegment "problems" idPathSegmentEncoder
      FrontendRoute_Topics -> Ob.PathSegment "topics" $
        let
          topicsRouteEncoder = Ob.pathComponentEncoder $ \case
            TopicsRoute_Problems -> Ob.PathSegment "problems" $ Ob.unitEncoder mempty
            TopicsRoute_ProblemSets -> Ob.PathSegment "problem-sets" $ Ob.unitEncoder mempty
        in
          Ob.pathSegmentEncoder
          . bimap Ob.unsafeTshowEncoder (Ob.maybeEncoder (Ob.unitEncoder mempty) topicsRouteEncoder)
  )

concat <$> mapM Ob.deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''Api
  , ''Api_Topics
  , ''TopicsRoute
  ]
