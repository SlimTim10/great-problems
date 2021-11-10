module Problem.Compile
  ( widget
  , performRequest
  , performRequestWithId
  , Randomize(..)
  , Request(..)
  ) where

import Common.Lib.Prelude

import qualified Data.Map as Map
import qualified GHCJS.DOM.Types
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import qualified Frontend.Lib.Util as Util
import qualified Frontend.Lib.SessionStorage as SessionStorage
import qualified Common.Route as Route
import qualified Common.Api.Compile as Compile
import qualified Widget.Button as Button
import qualified Problem.Loading as Loading
import qualified Problem.FormFile as FormFile

data Randomize = Randomize | Reset | NoChange

data Request = Request
  { contents :: Text
  , randomizeVariables :: Randomize
  , outputOption :: Compile.OutputOption
  , figures :: [FormFile.FormFile]
  }

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
  => R.Dynamic t Request
  -> m (R.Dynamic t (Loading.WithLoading (Maybe Compile.Response))) -- ^ Response
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
  -> R.Dynamic t Request
  -> m (R.Dynamic t (Loading.WithLoading (Maybe Compile.Response))) -- ^ Response
performRequest e compileRequest = do
  formData :: R.Event t (Map Text (R'.FormValue GHCJS.DOM.Types.File)) <- R.performEvent
    $ R.ffor (R.tagPromptlyDyn compileRequest e) $ \req -> do
    randomizeVariablesParam <- parseRandomize $ randomizeVariables req
    let formDataParams :: Map Compile.RequestParam (R'.FormValue GHCJS.DOM.Types.File) = (
          Compile.ParamContents =: R'.FormValue_Text (contents req)
            <> Compile.ParamRandomizeVariables =: R'.FormValue_Text randomizeVariablesParam
            <> Compile.ParamOutputOption =: R'.FormValue_Text (cs . show . outputOption $ req)
            <> Compile.ParamFigures =: R'.FormValue_List (map Util.formFile . figures $ req)
          )
    return $ Map.mapKeys (cs . show) formDataParams

  response :: R.Event t Text <- Util.postForm
    (Route.apiHref $ Route.Api_Compile :/ Nothing)
    formData
  compileResponse :: R.Dynamic t (Maybe Compile.Response) <- R.holdDyn Nothing
    $ R.decodeText <$> response
  loading <- compileResponse `Util.notUpdatedSince` e
  return $ Loading.WithLoading <$> compileResponse <*> loading

performRequestWithId
  :: forall t m.
     ( R.MonadHold t m
     , R.PerformEvent t m
     , R.HasJSContext (R.Performable m)
     , JS.MonadJSM (R.Performable m)
     , R.TriggerEvent t m
     , MonadFix m
     )
  => R.Event t () -- ^ Event to trigger request
  -> Integer -- ^ Problem ID
  -> R.Dynamic t Problem.Compile.Request
  -> m (R.Dynamic t (Loading.WithLoading (Maybe Compile.Response))) -- ^ Response
performRequestWithId e problemId compileRequest = do
  formData :: R.Event t (Map Text (R'.FormValue GHCJS.DOM.Types.File)) <- R.performEvent
    $ R.ffor (R.tagPromptlyDyn compileRequest e) $ \req -> do
    randomizeVariablesParam <- parseRandomize $ randomizeVariables req
    let formDataParams :: Map Compile.RequestParam (R'.FormValue GHCJS.DOM.Types.File) = (
          Compile.ParamRandomizeVariables =: R'.FormValue_Text randomizeVariablesParam
            <> Compile.ParamOutputOption =: R'.FormValue_Text (cs . show . Problem.Compile.outputOption $ req)
          )
    return $ Map.mapKeys (cs . show) formDataParams
    
  rawCompileResponse :: R.Event t Text <- Util.postForm
    (Route.apiHref $ Route.Api_Compile :/ Just problemId)
    formData
  compileResponse :: R.Dynamic t (Maybe Compile.Response) <- R.holdDyn Nothing
    $ R.decodeText <$> rawCompileResponse
  loading :: R.Dynamic t Bool <- compileResponse `Util.notUpdatedSince` e
  return $ Loading.WithLoading <$> compileResponse <*> loading

parseRandomize
  :: JS.MonadJSM m
  => Randomize
  -> m Text
parseRandomize = \case
  Randomize -> do
    x <- Util.random32
    SessionStorage.setItem "randomSeed" (cs . show $ x)
    return . cs . show $ x
  Reset -> do
    let x = 0 :: Integer
    SessionStorage.setItem "randomSeed" (cs . show $ x)
    return . cs . show $ x
  NoChange -> SessionStorage.getItem "randomSeed" >>= \case
    Nothing -> return . cs . show $ (0 :: Integer)
    Just x -> return x
