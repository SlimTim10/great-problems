module VerifyEmail
  ( widget
  ) where

import qualified Control.Monad.IO.Class as IO
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.TriggerEvent t m
     , R.MonadHold t m
     , R.PostBuild t m
     , IO.MonadIO m
     )
  => R.Dynamic t Text
  -> m ()
widget secret = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    onload :: R.Event t () <- R.getPostBuild
    let url :: R.Dynamic t Text = fmap (\x -> Route.apiHref (Route.Api_VerifyEmail :/ x)) secret
    let endpoint :: R.Event t Text = R.tagPromptlyDyn url onload
    response :: R.Event t (Maybe Error.Error) <- R.getAndDecode endpoint

    errorText :: R.Dynamic t Text <- R.holdDyn ""
      $ fromMaybe mempty . (fmap Error.message)
      <$> response
    R.el "p" $ R.dynText errorText

    successText :: R.Dynamic t Text <- R.holdDyn ""
      $ (\case
            Nothing -> "Thank you for completing the registration process. You can now sign in to start exploring Great Problems!"
            Just _ -> ""
        ) . (fmap Error.message)
      <$> response
    R.el "p" $ R.dynText successText
