module Problem.ErrorsToggle
  ( widget
  ) where

import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util
import qualified Common.Api.Error as Error

widget
  :: forall t m a.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , MonadFix m
     , R.PostBuild t m
     )
  => R.Dynamic t (Maybe (Either Error.Error Text)) -- ^ Response from compile attempt, which should contain the errors to show
  -> R.Event t a -- ^ Force reset
  -> m (R.Dynamic t Bool)
widget compileResponse reset = do
  rec
    e <- Util.buttonDynClass "Error log" $ style <$> btnOrReset <*> compileResponse
    btnOrReset <- R.foldDyn ($) False $ R.leftmost [not <$ e, const False <$ reset]
  return $ showErrors <$> btnOrReset <*> compileResponse
  where
    style
      :: Bool -- ^ Active toggled
      -> Maybe (Either Error.Error Text) -- ^ Response from compile request
      -> Text
    style False (Just (Right "")) = attention
    style False (Just (Left _)) = attention
    style True _ = active
    style False _ = ready
    attention = "border border-brand-primary rounded bg-red-200 text-blue-700 font-medium px-2 py-1 text-brand-sm"
    ready = "border border-brand-primary rounded bg-brand-primary text-white font-medium px-2 py-1 text-brand-sm"
    active = "border border-brand-primary rounded bg-white text-blue-700 font-medium px-2 py-1 text-brand-sm"

showErrors
  :: Bool -- ^ Error pane is toggled
  -> Maybe (Either Error.Error Text) -- ^ Compile response
  -> Bool
showErrors True _ = True
showErrors False Nothing = False
showErrors False (Just (Right "")) = True
showErrors False (Just (Left _)) = True
showErrors False (Just (Right _)) = False
