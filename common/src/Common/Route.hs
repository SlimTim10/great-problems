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

import Control.Category ((>>>), (.), id)
import Control.Categorical.Bifunctor (bimap)
import Data.Text (Text)
import Data.Functor.Identity (Identity)
import Obelisk.Route ( pattern (:/) )
import qualified Obelisk.Route as Ob
import qualified Obelisk.Route.TH as Ob
import qualified Data.Map as Map

import Global

type Query = Map Text (Maybe Text)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute (Ob.R Api)

data Api :: * -> * where
  -- Api_Problems :: Api (Maybe Integer, Map Text (Maybe Text))
  Api_Problems :: Api (Maybe Integer, Query)
  Api_Topics :: Api Query
  Api_Users :: Api (Maybe Integer)
  Api_TopicHierarchy :: Api (Maybe Integer)

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Explore :: FrontendRoute (Maybe (Ob.R ExploreRoute))
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_SignIn :: FrontendRoute ()
  FrontendRoute_New :: FrontendRoute ()
  FrontendRoute_ViewProblem :: FrontendRoute Integer
  FrontendRoute_ViewProblemSet :: FrontendRoute Integer
  FrontendRoute_ViewUser :: FrontendRoute Integer
  FrontendRoute_Topics :: FrontendRoute (Integer, Ob.R TopicsRoute)

data ExploreRoute :: * -> * where
  ExploreRoute_Problems :: ExploreRoute ()
  ExploreRoute_ProblemSets :: ExploreRoute ()

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
        Api_Problems -> Ob.PathSegment "problems" $ Ob.pathSegmentEncoder .
          bimap (Ob.maybeEncoder (Ob.unitEncoder mempty) Ob.unsafeTshowEncoder) Ob.queryOnlyEncoder
        -- Api_Problems -> Ob.PathSegment "problem-sets" $ Ob.pathSegmentEncoder .
        --   bimap (Ob.maybeEncoder (Ob.unitEncoder mempty) Ob.unsafeTshowEncoder) Ob.queryOnlyEncoder
        Api_Topics -> Ob.PathSegment "topics" Ob.queryOnlyEncoder
        Api_Users -> Ob.PathSegment "users" $
          Ob.maybeEncoder (Ob.unitEncoder mempty) $ idPathSegmentEncoder
        Api_TopicHierarchy -> Ob.PathSegment "topic-hierarchy" $
          Ob.maybeEncoder (Ob.unitEncoder mempty) $ idPathSegmentEncoder
  )
  (\case
      FrontendRoute_Home -> Ob.PathEnd $ Ob.unitEncoder mempty
      FrontendRoute_Explore -> Ob.PathSegment "explore" $
        Ob.maybeEncoder (Ob.unitEncoder mempty) $ Ob.pathComponentEncoder $ \case
        ExploreRoute_Problems -> Ob.PathSegment "problems" $ Ob.unitEncoder mempty
        ExploreRoute_ProblemSets -> Ob.PathSegment "problem-sets" $ Ob.unitEncoder mempty
      FrontendRoute_Register -> Ob.PathSegment "register" $ Ob.unitEncoder mempty
      FrontendRoute_SignIn -> Ob.PathSegment "sign-in" $ Ob.unitEncoder mempty
      FrontendRoute_New -> Ob.PathSegment "new" $ Ob.unitEncoder mempty
      FrontendRoute_ViewProblem -> Ob.PathSegment "problems" idPathSegmentEncoder
      FrontendRoute_ViewProblemSet -> Ob.PathSegment "problem-sets" idPathSegmentEncoder
      FrontendRoute_ViewUser -> Ob.PathSegment "users" idPathSegmentEncoder
      FrontendRoute_Topics -> Ob.PathSegment "topics" $ Ob.pathSegmentEncoder .
        bimap Ob.unsafeTshowEncoder (Ob.pathComponentEncoder $ \case
          TopicsRoute_Problems -> Ob.PathSegment "problems" $ Ob.unitEncoder mempty
          TopicsRoute_ProblemSets -> Ob.PathSegment "problem-sets" $ Ob.unitEncoder mempty)
  )

apiHref
  :: Ob.R Api
  -> Text
apiHref r = Ob.renderBackendRoute checkedEncoder $ BackendRoute_Api :/ r

checkedEncoder :: Applicative check => Ob.Encoder check Identity (Ob.R (Ob.FullRoute BackendRoute FrontendRoute)) Ob.PageName
checkedEncoder = either (error "checkEncoder failed") id $ Ob.checkEncoder fullRouteEncoder

readParamFromQuery
  :: Read a
  => Text -- ^ Param name in route query
  -> Query
  -> Maybe a
readParamFromQuery param query = do
  x <- fromMaybe Nothing (Map.lookup param query)
  readMaybe (cs x)

textParamFromQuery
  :: Text -- ^ Param name in route query
  -> Query
  -> Maybe Text
textParamFromQuery param query = fromMaybe Nothing (Map.lookup param query)

concat <$> mapM Ob.deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''Api
  , ''TopicsRoute
  , ''ExploreRoute
  ]
