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

import Prelude hiding (id, (.))
import Common.Lib.Prelude

import Control.Category ((>>>), (.), id)
import Control.Categorical.Bifunctor (bimap)
import Data.Functor.Identity (Identity)
import qualified Obelisk.Route as Ob
import qualified Obelisk.Route.TH as Ob
import qualified Data.Map as Map

import qualified Common.Api.MetaSetting as MetaSetting

type Query = Map Text (Maybe Text)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute (Ob.R Api)

data Api :: * -> * where
  Api_Problems :: Api (Maybe Integer, Query)
  Api_Figures :: Api Integer
  Api_Topics :: Api (Maybe Integer, Query)
  Api_TopicHierarchy :: Api (Maybe Integer)
  Api_Users :: Api (Maybe Integer)
  Api_Register :: Api ()
  Api_VerifyEmail :: Api Text
  Api_ChangePassword :: Api ()
  Api_ResetPassword :: Api ()
  Api_ResendEmail :: Api ()
  Api_SignIn :: Api ()
  Api_SignOut :: Api ()
  Api_DuplicateProblem :: Api Integer
  Api_Compile :: Api (Maybe Integer)
  Api_Roles :: Api ()
  Api_MetaSettings :: Api (Maybe MetaSetting.Setting)

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_VerifyEmail :: FrontendRoute Text
  FrontendRoute_SignIn :: FrontendRoute ()
  FrontendRoute_SignOut :: FrontendRoute ()
  FrontendRoute_ForgotPassword :: FrontendRoute ()
  FrontendRoute_ResetPassword :: FrontendRoute Text
  FrontendRoute_ResendEmail :: FrontendRoute ()
  FrontendRoute_Settings :: FrontendRoute ()
  FrontendRoute_Problems :: FrontendRoute (Integer, Ob.R ProblemsRoute)
  FrontendRoute_NewProblem :: FrontendRoute ()
  FrontendRoute_DuplicateProblem :: FrontendRoute Integer
  FrontendRoute_ViewProblemSet :: FrontendRoute Integer
  FrontendRoute_Profile :: FrontendRoute Integer
  FrontendRoute_Admin :: FrontendRoute ()
  FrontendRoute_Search :: FrontendRoute Query

data ProblemsRoute :: * -> * where
  ProblemsRoute_View :: ProblemsRoute ()
  ProblemsRoute_Edit :: ProblemsRoute ()

idPathSegmentEncoder
  :: Ob.Encoder (Either Text) (Either Text) Integer Ob.PageName
idPathSegmentEncoder = Ob.unsafeTshowEncoder >>> Ob.singlePathSegmentEncoder

settingPathSegmentEncoder
  :: Ob.Encoder (Either Text) (Either Text) MetaSetting.Setting Ob.PageName
settingPathSegmentEncoder = Ob.unsafeTshowEncoder >>> Ob.singlePathSegmentEncoder

fullRouteEncoder
  :: Ob.Encoder (Either Text) Identity (Ob.R (Ob.FullRoute BackendRoute FrontendRoute)) Ob.PageName
fullRouteEncoder = Ob.mkFullRouteEncoder
  (Ob.FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> Ob.PathSegment "missing" $ Ob.unitEncoder mempty
      BackendRoute_Api -> Ob.PathSegment "api" $ Ob.pathComponentEncoder $ \case
        Api_Problems -> Ob.PathSegment "problems" $ Ob.pathSegmentEncoder .
          bimap (Ob.maybeEncoder (Ob.unitEncoder mempty) Ob.unsafeTshowEncoder) Ob.queryOnlyEncoder
        -- Api_ProblemSets -> Ob.PathSegment "problem-sets" $ Ob.pathSegmentEncoder .
        --   bimap (Ob.maybeEncoder (Ob.unitEncoder mempty) Ob.unsafeTshowEncoder) Ob.queryOnlyEncoder
        Api_Figures -> Ob.PathSegment "figures" $ idPathSegmentEncoder
        Api_Topics -> Ob.PathSegment "topics" $ Ob.pathSegmentEncoder .
          bimap (Ob.maybeEncoder (Ob.unitEncoder mempty) Ob.unsafeTshowEncoder) Ob.queryOnlyEncoder
        Api_Users -> Ob.PathSegment "users" $
          Ob.maybeEncoder (Ob.unitEncoder mempty) $ idPathSegmentEncoder
        Api_TopicHierarchy -> Ob.PathSegment "topic-hierarchy" $
          Ob.maybeEncoder (Ob.unitEncoder mempty) $ idPathSegmentEncoder
        Api_Register -> Ob.PathSegment "register" $ Ob.unitEncoder mempty
        Api_VerifyEmail -> Ob.PathSegment "verify-email" Ob.singlePathSegmentEncoder
        Api_ChangePassword -> Ob.PathSegment "change-password" $ Ob.unitEncoder mempty
        Api_ResetPassword -> Ob.PathSegment "reset-password" $ Ob.unitEncoder mempty
        Api_ResendEmail -> Ob.PathSegment "resend-email" $ Ob.unitEncoder mempty
        Api_SignIn -> Ob.PathSegment "sign-in" $ Ob.unitEncoder mempty
        Api_SignOut -> Ob.PathSegment "sign-out" $ Ob.unitEncoder mempty
        Api_DuplicateProblem -> Ob.PathSegment "duplicate-problem" $ idPathSegmentEncoder
        Api_Compile -> Ob.PathSegment "compile" $
          Ob.maybeEncoder (Ob.unitEncoder mempty) $ idPathSegmentEncoder
        Api_Roles -> Ob.PathSegment "roles" $ Ob.unitEncoder mempty
        Api_MetaSettings -> Ob.PathSegment "meta-settings" $
          Ob.maybeEncoder (Ob.unitEncoder mempty) $ settingPathSegmentEncoder
  )
  (\case
      FrontendRoute_Home -> Ob.PathEnd $ Ob.unitEncoder mempty
      FrontendRoute_Register -> Ob.PathSegment "register" $ Ob.unitEncoder mempty
      FrontendRoute_VerifyEmail -> Ob.PathSegment "verify-email" Ob.singlePathSegmentEncoder
      FrontendRoute_ResetPassword -> Ob.PathSegment "reset-password" Ob.singlePathSegmentEncoder
      FrontendRoute_SignIn -> Ob.PathSegment "sign-in" $ Ob.unitEncoder mempty
      FrontendRoute_SignOut -> Ob.PathSegment "sign-out" $ Ob.unitEncoder mempty
      FrontendRoute_ForgotPassword -> Ob.PathSegment "forgot-password" $ Ob.unitEncoder mempty
      FrontendRoute_ResendEmail -> Ob.PathSegment "resend-email" $ Ob.unitEncoder mempty
      FrontendRoute_Settings -> Ob.PathSegment "settings" $ Ob.unitEncoder mempty
      FrontendRoute_NewProblem -> Ob.PathSegment "new-problem" $ Ob.unitEncoder mempty
      FrontendRoute_DuplicateProblem -> Ob.PathSegment "duplicate-problem" $ idPathSegmentEncoder
      FrontendRoute_Problems -> Ob.PathSegment "problems" $ Ob.pathSegmentEncoder .
        bimap Ob.unsafeTshowEncoder
        (Ob.pathComponentEncoder $ \case
            ProblemsRoute_View -> Ob.PathEnd $ Ob.unitEncoder mempty
            ProblemsRoute_Edit -> Ob.PathSegment "edit" $ Ob.unitEncoder mempty
        )
      FrontendRoute_ViewProblemSet -> Ob.PathSegment "problem-sets" $ idPathSegmentEncoder
      FrontendRoute_Profile -> Ob.PathSegment "users" $ idPathSegmentEncoder
      FrontendRoute_Admin -> Ob.PathSegment "admin" $ Ob.unitEncoder mempty
      FrontendRoute_Search -> Ob.PathSegment "search" $ Ob.queryOnlyEncoder
  )

apiHref
  :: Ob.R Api
  -> Text
apiHref r = Ob.renderBackendRoute checkedEncoder $ BackendRoute_Api :/ r

frontendHref
  :: Ob.R FrontendRoute
  -> Text
frontendHref r = Ob.renderFrontendRoute checkedEncoder r

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
  , ''ProblemsRoute
  ]
