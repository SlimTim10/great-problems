{-# LANGUAGE PackageImports #-}
module Settings
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Web.KeyCode as Key
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
import qualified Obelisk.Route.Frontend as Ob
import qualified "ghcjs-dom" GHCJS.DOM.Document as DOM

import qualified Common.Route as Route
import qualified Common.Api.Problem as Problem
import qualified Common.Api.User as User
import qualified Common.Api.Request.ChangePassword as ChangePassword
import qualified Common.Api.Error as Error
import qualified Common.Api.ProblemStatus as ProblemStatus
import qualified Frontend.Lib.Api as Api
import qualified Widget.Button as Button
import qualified Widget.Input as Input
import qualified Widget.Spinner as Spinner
import qualified ProblemCards as ProblemCards

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , R.PerformEvent t m
     , R.MonadHold t m
     , R.PostBuild t m
     , MonadFix m
     , R.TriggerEvent t m
     , R.HasJSContext (R.Performable m)
     , JS.MonadJSM (R.Performable m)
     , JS.MonadJSM m
     , R.HasDocument m
     , DOM.IsDocument (R.RawDocument (R.DomBuilderSpace m))
     )
  => m ()
widget = do
  Util.getCurrentUser >>= \case
    Nothing -> do
      onload <- R.getPostBuild
      Ob.setRoute $ Route.FrontendRoute_Explore :/ Nothing <$ onload
    Just user -> widget' user

widget'
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , MonadFix m
     , R.MonadHold t m
     , JS.MonadJSM (R.Performable m)
     , JS.MonadJSM m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , R.PerformEvent t m
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     )
  => User.User
  -> m ()
widget' user = do
  R.elClass "div" "flex flex-col items-center mx-20" $ do
    R.elClass "div" "w-full" $ do
      
      section "Drafts" $ do
        drafts <- getDrafts (User.id user)
        R.elClass "div" "flex flex-col gap-2" $ do
          void $ R.simpleList drafts
            $ ProblemCards.problemCardWidget
            ProblemCards.Options { ProblemCards.showAuthor = False, ProblemCards.linkEdit = True }

      changePasswordSection

      section "Sign out" $ do
        Ob.routeLink (Route.FrontendRoute_SignOut :/ ()) $ do
          Button.secondary "Sign out"

  where
    section txt body = do
      R.elClass "div" "mt-10" $ do
        R.elClass "p" "text-brand-lg border-b border-brand-black mb-4" $ R.text txt
        body

    changePasswordSection = do
      section "Change password" $ do
        R.elClass "div" "flex flex-col gap-4 w-96" $ mdo
          
          oldPassword :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
            R.elClass "p" "" $ R.text "Old password"
            Input.passwordClass "border px-1"
          
          newPassword :: R.Dynamic t Text <- R.elClass "div" "flex justify-between" $ do
            R.elClass "p" "" $ R.text "New password"
            Input.passwordClass "border px-1"
            
          confirmNewPasswordInput <- R.elClass "div" "flex justify-between" $ do
            R.elClass "p" "" $ R.text "Confirm new password"
            Input.rawPasswordClass "border px-1"
          let confirmNewPassword :: R.Dynamic t Text = R.value confirmNewPasswordInput
          
          R.dyn_ passwordMatchError
          R.dyn_ passwordEmptyError
          
          changePasswordButton :: R.Event t () <- Button.primary' "Save"

          spinner <- Spinner.holdSmall triggerRequest
          status <- R.holdDyn R.blank . R.leftmost . map R.updated $ [spinner, message]
          R.dyn_ status
          
          let changePassword = R.leftmost
                [ changePasswordButton
                , R.keydown Key.Enter confirmNewPasswordInput
                ]
                
          let passwordMatch :: R.Dynamic t Bool = (==) <$> newPassword <*> confirmNewPassword
          passwordMatchError <- R.holdDyn R.blank $ R.ffor (R.tagPromptlyDyn passwordMatch changePassword) $ \case
            True -> R.blank
            False -> R.elClass "p" "text-red-500" $ R.text "Password must match"

          let passwordNotEmpty :: R.Dynamic t Bool = (/= "") <$> newPassword
          passwordEmptyError <- R.holdDyn R.blank $ R.ffor (R.tagPromptlyDyn passwordNotEmpty changePassword) $ \case
            True -> R.blank
            False -> R.elClass "p" "text-red-500" $ R.text "Password cannot be empty"

          let triggerRequest = () <$
                R.ffilter
                and
                (R.tagPromptlyDyn (R.distributeListOverDyn [passwordMatch, passwordNotEmpty]) changePassword)

          response :: R.Event t (Either Error.Error ()) <- Api.postRequest
            (R.zipDyn oldPassword newPassword)
            triggerRequest
            (Route.Api_ChangePassword :/ ())
            (\(oldPassword', newPassword') ->
               ChangePassword.ChangePassword (ChangePassword.OldPassword oldPassword') newPassword')

          message :: R.Dynamic t (m ()) <- R.holdDyn R.blank
            $ R.ffor response $ \case
            Left e -> do
              R.elClass "p" "text-red-500" $ R.text (Error.message e)
            Right _ -> do
              R.elClass "p" "text-green-600" $ R.text "Your password has been changed."
          
          return ()


getDrafts
  :: ( R.PostBuild t m
     , JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     , JS.MonadJSM m
     )
  => Integer
  -> m (R.Dynamic t [Problem.Problem])
getDrafts authorId = do
  response :: R.Event t (Maybe [Problem.Problem]) <- Util.getOnload
    $ Route.apiHref
    $ Route.Api_Problems :/
    ( Nothing, Problem.getParamsToRouteQuery
      $ Problem.GetParams
      { Problem.gpTopic = Nothing
      , Problem.gpAuthor = Just authorId
      , Problem.gpStatus = Just (fromIntegral . fromEnum $ ProblemStatus.Draft)
      }
    )
  R.holdDyn [] $ fromMaybe [] <$> response
