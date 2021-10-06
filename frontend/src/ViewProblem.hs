module ViewProblem
  ( widget
  ) where

import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle as JS
import qualified JSDOM.Types
import qualified Data.CaseInsensitive as CI
-- import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R
-- Import patch
import qualified MyReflex.Dom.Xhr.FormData as R'

import qualified Common.Route as Route
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Problem as Problem
import qualified Common.Api.User as User
import qualified Problem.PdfViewer as PdfViewer
import qualified Widget.Button as Button
import qualified Widget.Input as Input
import qualified Problem.Loading as Loading
import qualified Util
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , R.MonadSample t (R.Performable m)
     )
  => Integer
  -> m ()
widget problemId = mdo
  problem :: R.Dynamic t (Maybe Problem.Problem) <- do
    r :: R.Event t (Maybe Problem.Problem) <- Util.getOnload
      $ Route.apiHref $ Route.Api_Problems :/
      ( Just problemId, Problem.getParamsToRouteQuery
        $ Problem.GetParams
        { Problem.gpExpand = Just ["author", "topic"]
        , Problem.gpInclude = Just Problem.TopicPath
        , Problem.gpTopic = Nothing
        }
      )
    R.holdDyn Nothing r

  onload :: R.Event t () <- R.getPostBuild
  onloadAction :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
    performRequest onload $ Compile.Request
      <$> R.constDyn ""
      <*> R.constDyn False
      <*> R.constDyn Compile.QuestionOnly
      <*> R.constDyn []

  let actions =
        [ onloadAction
        , randomizeVariablesAction
        , resetVariablesAction
        , showAnswerAction
        , showSolutionAction
        ]

  latestResponse :: R.Dynamic t (Maybe Compile.Response) <- R.holdDyn Nothing
    $ Loading.latestAction actions

  anyLoading :: R.Dynamic t Bool <- Loading.anyLoading actions

  ( randomizeVariablesAction
    , resetVariablesAction
    , showAnswerAction
    , showSolutionAction
    ) <- R.elClass "div" "w-brand-screen-lg flex" $ mdo
    ( randomizeVariablesAction
      , resetVariablesAction
      , showAnswerAction
      , showSolutionAction
      ) <- R.elClass "div" "w-96 flex-none flex flex-col gap-2" $ mdo
      R.elClass "div" "pb-3 border-b border-brand-light-gray flex flex-col gap-1" $ do
        let problemDetails = \p -> do
              R.elClass "p" "text-brand-sm text-brand-gray" $ do
                R.text $ "#" <> (cs . show . Problem.id $ p)
              R.elClass "p" "font-medium" $ do
                R.text $ Problem.summary p
              R.elClass "p" "text-brand-sm text-brand-gray" $ do
                R.text $ "Last updated at " <> (cs . show $ Problem.updatedAt p)
              R.elClass "div" "flex gap-1" $ do
                R.elClass "p" "text-brand-sm text-brand-gray" $ do
                  R.text $ "by"
                R.elClass "p" "text-brand-sm text-brand-gray font-bold" $ do
                  R.text $ either (const "") (CI.original . User.fullName) (Problem.author p)
        R.dyn_ $ maybe R.blank problemDetails <$> problem
      R.elClass "div" "pt-1 pb-3 border-b border-brand-light-gray flex gap-6" $ do
        R.elClass "p" "text-brand-sm text-brand-primary font-medium" $ do
          R.text "SHARE"
        R.elClass "p" "text-brand-sm text-brand-primary font-medium" $ do
          R.text "SAVE"
      (randomizeVariablesAction, resetVariablesAction) <- R.elClass "div" "py-3 flex gap-2" $ mdo
        randomizeVariables :: R.Event t () <- Button.primarySmallClass'
          "Randomize variables"
          "active:bg-blue-400"
        randomizeVariablesAction :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
          performRequest randomizeVariables $ Compile.Request
            <$> R.constDyn ""
            <*> R.constDyn True
            <*> outputOption
            <*> R.constDyn []
        resetVariables :: R.Event t () <- Button.primarySmallClass'
          "Reset variables"
          "active:bg-blue-400"
        resetVariablesAction :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
          performRequest resetVariables $ Compile.Request
            <$> R.constDyn ""
            <*> R.constDyn False
            <*> outputOption
            <*> R.constDyn []
        return (randomizeVariablesAction, resetVariablesAction)
      (showAnswerAction, showSolutionAction, outputOption) <- R.elClass "div" "flex gap-4" $ do
        R.elClass "p" "font-medium text-brand-primary"
          $ R.text "Show problem with:"
        R.elClass "div" "flex flex-col" $ do
          showAnswer :: R.Dynamic t Bool <- Input.checkboxClass
            "cursor-pointer mr-2 checkbox-brand-primary"
            "font-medium text-brand-primary cursor-pointer"
            "Answer"
          showAnswerAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
            performRequest (R.updated $ const () <$> showAnswer) $ Compile.Request
              <$> R.constDyn ""
              <*> R.constDyn False
              <*> outputOption
              <*> R.constDyn []
          showSolution :: R.Dynamic t Bool <- Input.checkboxClass
            "cursor-pointer mr-2 checkbox-brand-primary"
            "font-medium text-brand-primary cursor-pointer"
            "Solution"
          showSolutionAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
            performRequest (R.updated $ const () <$> showSolution) $ Compile.Request
              <$> R.constDyn ""
              <*> R.constDyn False
              <*> outputOption
              <*> R.constDyn []
          let outputOption' :: R.Dynamic t Compile.OutputOption =
                (\showAnswer' showSolution' ->
                   case (showAnswer', showSolution') of
                     (False, False) -> Compile.QuestionOnly
                     (False, True) -> Compile.WithSolution
                     (True, False) -> Compile.WithAnswer
                     (True, True) -> Compile.WithSolutionAndAnswer
                ) <$> showAnswer <*> showSolution
          return (showAnswerAction', showSolutionAction', outputOption')
      return
        ( randomizeVariablesAction
        , resetVariablesAction
        , showAnswerAction
        , showSolutionAction
        )

    R.elClass "div" "pl-2 flex-1 h-full flex flex-col" $ do
      let errorsToggle :: R.Dynamic t Bool = R.constDyn False
      R.elClass "div" "flex-1" $ PdfViewer.widget latestResponse anyLoading errorsToggle

    return
      ( randomizeVariablesAction
      , resetVariablesAction
      , showAnswerAction
      , showSolutionAction
      )

  return ()
  where
    performRequest
      :: R.Event t () -- ^ Event to trigger request
      -> R.Dynamic t Compile.Request
      -> m (R.Dynamic t (Loading.WithLoading (Maybe Compile.Response))) -- ^ Response
    performRequest e compileRequest = do
      formData :: R.Event t (Map Text (R'.FormValue JSDOM.Types.File)) <- R.performEvent
        $ R.ffor (R.tagPromptlyDyn compileRequest e) $ \req -> do
        let
          formDataParams :: Map Compile.RequestParam (R'.FormValue JSDOM.Types.File) = (
            Compile.ParamRandomizeVariables =: R'.FormValue_Text
              (Util.formBool . Compile.randomizeVariables $ req)
            <> Compile.ParamOutputOption =: R'.FormValue_Text
              (cs . show . Compile.outputOption $ req)
            )
          formDataText = Map.mapKeys (cs . show) formDataParams
        return formDataText
    
      rawCompileResponse :: R.Event t Text <- Util.postForm
        (Route.apiHref $ Route.Api_Compile :/ Just problemId)
        formData
      compileResponse :: R.Dynamic t (Maybe Compile.Response) <- R.holdDyn Nothing
        $ R.decodeText <$> rawCompileResponse
      -- The response is loading when the event has been triggered and the response has yet to update
      loading :: R.Dynamic t Bool <- R.zipDynWith
        (\(x :: Integer) (y :: Integer) -> x > 0 && x > y)
        <$> R.count e <*> R.count (R.updated compileResponse)
      -- loading :: R.Dynamic t Bool <- Util.updatedAfter compileResponse e
      return $ Loading.WithLoading <$> compileResponse <*> loading
