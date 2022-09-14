module Problem.Compile
  ( widget
  , performRequest
  , performRequestWithId
  , Randomize(..)
  , Request(..)
  , Response(..)
  , mkRequest
  ) where

import Common.Lib.Prelude

import qualified Control.Monad.IO.Class as IO
import qualified Data.Time.Clock as Time
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
import qualified Common.Api.Error as Error
import qualified Widget.Button as Button
import qualified Problem.Loading as Loading
import qualified Problem.FormFile as FormFile

data Randomize = Randomize | Reset | NoChange
  deriving (Show, Eq)

data Request = Request
  { contents :: Text
  , randomizeVariables :: Randomize
  , figures :: [FormFile.FormFile]
  , time :: Time.UTCTime
  } deriving (Show, Eq)

-- | Keep request time together with response
-- so we can prioritize responses based on request time
data Response = Response
  { reqTime :: Time.UTCTime
  , response :: Maybe (Either Error.Error Text)
  } deriving (Show, Eq)

widget
  :: ( R.DomBuilder t m
     )
  => m (R.Event t ())
widget = Button.primarySmallClass' "Compile" "active:bg-blue-400"

performRequest
  :: forall t m.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , MonadFix m
     , JS.MonadJSM m
     )
  => R.Event t Request
  -> m (R.Dynamic t (Loading.WithLoading Response)) -- ^ Response
performRequest compileRequest = do
  formData :: R.Event t (Map Text (R'.FormValue GHCJS.DOM.Types.File)) <- R.performEvent
    $ R.ffor compileRequest $ \req -> do
    randomizeVariablesParam <- parseRandomize $ randomizeVariables req
    let formDataParams :: Map Compile.RequestParam (R'.FormValue GHCJS.DOM.Types.File) = (
          Compile.ParamContents =: R'.FormValue_Text (contents req)
            <> Compile.ParamRandomizeVariables =: R'.FormValue_Text randomizeVariablesParam
            <> Compile.ParamFigures =: R'.FormValue_List (map Util.formFile . figures $ req)
          )
    return $ Map.mapKeys (cs . show) formDataParams

  rawCompileResponse :: R.Event t Text <- Util.postForm
    (Route.apiHref $ Route.Api_Compile :/ Nothing)
    formData
  let err :: R.Event t (Maybe Error.Error) = R.decodeText <$> rawCompileResponse
  let resErr :: R.Event t (Either Error.Error Text) = Left . fromJust
        <$> R.ffilter isJust err

  let success :: R.Event t (Maybe Text) = R.decodeText <$> rawCompileResponse
  let resSuccess :: R.Event t (Either Error.Error Text) = Right . fromJust
        <$> R.ffilter isJust success

  compileResponse :: R.Dynamic t (Maybe (Either Error.Error Text)) <-
    R.holdDyn Nothing $ Just <$> R.leftmost [resErr, resSuccess]
  loading :: R.Dynamic t Bool <- compileResponse `Util.notUpdatedSince` compileRequest
  ct <- IO.liftIO Time.getCurrentTime
  t <- R.holdDyn ct (time <$> compileRequest)
  return $ Loading.WithLoading <$> R.zipDynWith Response t compileResponse <*> loading

performRequestWithId
  :: forall t m.
     ( R.MonadHold t m
     , R.PerformEvent t m
     , R.HasJSContext (R.Performable m)
     , JS.MonadJSM (R.Performable m)
     , R.TriggerEvent t m
     , MonadFix m
     , JS.MonadJSM m
     )
  => Integer -- ^ Problem ID
  -> R.Event t Request
  -> m (R.Dynamic t (Loading.WithLoading Response)) -- ^ Response
performRequestWithId problemId compileRequest = do
  formData :: R.Event t (Map Text (R'.FormValue GHCJS.DOM.Types.File)) <- R.performEvent
    $ R.ffor compileRequest $ \req -> do
    randomizeVariablesParam <- parseRandomize $ randomizeVariables req
    let formDataParams :: Map Compile.RequestParam (R'.FormValue GHCJS.DOM.Types.File) = (
          Compile.ParamRandomizeVariables =: R'.FormValue_Text randomizeVariablesParam
          )
    return $ Map.mapKeys (cs . show) formDataParams
    
  rawCompileResponse :: R.Event t Text <- Util.postForm
    (Route.apiHref $ Route.Api_Compile :/ Just problemId)
    formData
  let err :: R.Event t (Maybe Error.Error) = R.decodeText <$> rawCompileResponse
  let resErr :: R.Event t (Either Error.Error Text) = Left . fromJust
        <$> R.ffilter isJust err

  let success :: R.Event t (Maybe Text) = R.decodeText <$> rawCompileResponse
  let resSuccess :: R.Event t (Either Error.Error Text) = Right . fromJust
        <$> R.ffilter isJust success

  compileResponse :: R.Dynamic t (Maybe (Either Error.Error Text)) <-
    R.holdDyn Nothing $ Just <$> R.leftmost [resErr, resSuccess]
  loading :: R.Dynamic t Bool <- compileResponse `Util.notUpdatedSince` compileRequest
  ct <- IO.liftIO Time.getCurrentTime
  t <- R.holdDyn ct (time <$> compileRequest)
  return $ Loading.WithLoading <$> R.zipDynWith Response t compileResponse <*> loading

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

mkRequest
  :: ( JS.MonadJSM (R.Performable m)
     , R.PerformEvent t m
     , R.MonadSample t (R.Performable m)
     )
  => R.Event t () -- ^ Event to trigger request
  -> R.Dynamic t Text -- ^ Problem contents
  -> R.Dynamic t Problem.Compile.Randomize -- ^ Randomize problem variables
  -> R.Dynamic t [FormFile.FormFile] -- ^ Problem figures
  -> m (R.Event t Problem.Compile.Request)
mkRequest e c rv figs = R.performEvent $ R.ffor e $ \_ -> do
  c' <- R.sample . R.current $ c
  rv' <- R.sample . R.current $ rv
  figs' <- R.sample . R.current $ figs
  t <- IO.liftIO Time.getCurrentTime
  return $ Problem.Compile.Request c' rv' figs' t
