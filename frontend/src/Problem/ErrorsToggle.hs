module Problem.ErrorsToggle
  ( widget
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T

import qualified Reflex.Dom.Core as R

import qualified Problem.Convert as Convert
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , MonadFix m
     , R.PostBuild t m
     )
  => R.Dynamic t (Maybe Convert.ConvertResponse)
  -> R.Dynamic t Bool
  -> m (R.Dynamic t Bool)
widget convertResponse reset = do
  rec
    e <- buttonDynClass "Errors" (cls <$> btnOrReset <*> hasErrors)
    btnOrReset <- R.foldDyn ($) False $ R.leftmost [not <$ e, const False <$ R.updated reset]
  return $ showErrors <$> btnOrReset <*> convertResponse
  where
    hasErrors :: R.Dynamic t Bool = f <$> convertResponse
    f :: Maybe Convert.ConvertResponse -> Bool
    f Nothing = False
    f (Just res) = not . T.null $ Convert.errorIcemaker res
    cls :: Bool -> Bool -> Text
    cls active b = bool "" "border-2 border-gray-800" active <> bool "" "bg-red-200" b

showErrors
  :: Bool
  -> Maybe Convert.ConvertResponse
  -> Bool
showErrors btn convertResponse
  | btn = True
  | Maybe.isNothing convertResponse = False
  | not . T.null $ pdfContent = False
  | T.null pdfContent = True
  | otherwise = False
  where
    pdfContent = Convert.pdfContent . Maybe.fromJust $ convertResponse

buttonDynClass
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     )
  => Text
  -> R.Dynamic t Text
  -> m (R.Event t ())
buttonDynClass t c = do
  (e, _) <- R.elDynClass' "button" c $ R.text t
  return $ R.domEvent R.Click e
