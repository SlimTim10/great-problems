module Problem.Compile
  ( widget
  , performRequest
  ) where

import qualified Data.Map as Map
import qualified JSDOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import qualified Common.Route as Route
import qualified Common.Api.Compile as Compile
import qualified Widget.Button as Button
import qualified Util
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , MonadFix m
     )
  => R.Dynamic t Compile.Request
  -> m (R.Dynamic t (Maybe Compile.Response, Bool)) -- ^ Response, loading
widget compileRequest = do
  compile :: R.Event t () <- Button.primarySmallClass' "Compile" "active:bg-blue-400"
  performRequest compile compileRequest

performRequest
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , MonadFix m
     )
  => R.Event t () -- ^ Event to trigger request
  -> R.Dynamic t Compile.Request
  -> m (R.Dynamic t (Maybe Compile.Response, Bool)) -- ^ Response, loading
performRequest e compileRequest = do
  formData :: R.Event t (Map Text (R'.FormValue JSDOM.Types.File)) <- R.performEvent
    $ R.ffor (R.tagPromptlyDyn compileRequest e) $ \req -> do
    let
      formDataParams :: Map Compile.RequestParam (R'.FormValue JSDOM.Types.File) = (
        Compile.ParamContent =: R'.FormValue_Text (Compile.content req)
        <> Compile.ParamRandomizeVariables =: R'.FormValue_Text (Util.formBool . Compile.randomizeVariables $ req)
        <> Compile.ParamOutputOption =: R'.FormValue_Text (cs . show . Compile.outputOption $ req)
        <> Compile.ParamFigures =: R'.FormValue_List (map Util.formFile . Compile.figures $ req)
        )
      formDataText = Map.mapKeys (cs . show) formDataParams
    return formDataText

  response :: R.Event t Text <- Util.postForm
    (Route.apiHref $ Route.Api_Compile :/ ())
    formData
  compileResponse :: R.Dynamic t (Maybe Compile.Response) <- R.holdDyn Nothing
    $ R.decodeText <$> response
  -- The response is loading when the event has been triggered and the response has yet to update
  loading <- R.zipDynWith
    (\(x :: Integer) (y :: Integer) -> x > 0 && x > y)
    <$> R.count e <*> R.count (R.updated compileResponse)
  return $ R.zipDyn compileResponse loading
