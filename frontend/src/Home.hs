module Home
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Data.Text as T
import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified MyReflex.Dom.Widget.Basic as R'
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.Problem as Problem
import qualified Common.Api.Compile as Compile
import qualified Widget.Button as Button
import qualified Widget.Input as Input
import qualified Problem.Loading as Loading
import qualified Problem.Compile
import qualified Problem.PdfViewer as PdfViewer
import qualified ViewProblem

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , Ob.RouteToUrl (Ob.R Route.FrontendRoute) m
     , Ob.SetRoute t (Ob.R Route.FrontendRoute) m
     , R.Prerender js t m
     , R.MonadSample t (R.Performable m)
     )
  => m ()
widget = do
  R.elClass "div" "flex flex-col items-center" $ do
    
    R.elClass "div" "mt-2 mx-20" $ do
      R.elClass "p" "mr-10 text-brand-lg font-bold" $ R.text "The best practice problems & problem sets for any topic"
      R.elClass "p" "mt-2" $ R.text "Discover problems & problem sets in topics like Math, Electrical Engineering, Physics, and more, made by real teachers."
      
    R.elClass "div" "mt-10 flex flex-col items-center gap-2 w-full" $ do
      Ob.routeLink (Route.FrontendRoute_Explore :/ Nothing) $ do
        Button.primary "Explore"
      R.elClass "div" "flex" $ do
        R.elClass "div" "flex items-center mr-2" $ do
          R.elClass "hr" "w-20 border-gray-400" R.blank
        R.elClass "p" "" $ R.text "or"
        R.elClass "div" "flex items-center ml-2" $ do
          R.elClass "hr" "w-20 border-gray-400" R.blank
      R.elClass "div" "flex justify-center w-full" $ do
        R'.elAttrClass
          "input"
          ("type" =: "search" <> "placeholder" =: "e.g. Newton's laws")
          "border rounded h-10 w-1/3 px-1 mr-1"
          $ R.blank
        Button.primary "Search"

    R.elClass "div" "mt-20 flex flex-col items-center gap-8" $ do
      R.elClass "p" "text-brand-lg font-bold" $ R.text "For students"
      R.elClass "div" "flex gap-8 flex-col xl:flex-row" $ do
        let card = \title body -> do
              R.elClass "div" "w-brand-card bg-brand-light-gray box-shadow-brand-card flex flex-col items-center p-4" $ do
                R.elClass "p" "font-medium" $ R.text title
                R.elClass "p" "mt-2 text-center text-brand-sm font-light" $ R.text body
        card "Perfect for practice" "Only see the solution when you want to. You can even repeat the same problem with randomized variables!"
        card "Trusted content" "Every problem was contributed by a real teacher, so you can be confident in the material and solution."
        card "Problem sets" "Find, create, and share problem sets for any topic. Practice with a great problem set to ace your next test!"

    R.elClass "div" "mt-20 w-full bg-brand-light-gray pb-20" $ do
      R.elClass "p" "m-1 text-brand-light-gray" $ R.text "EXAMPLE PROBLEM"
      R.elClass "div" "home-example-problem-container" $ do
        exampleProblem

    R.elClass "div" "mt-20 flex flex-col items-center gap-8" $ do
      R.elClass "p" "text-brand-lg font-bold" $ R.text "For teachers"
      R.elClass "div" "flex gap-8 flex-col xl:flex-row" $ do
        let card = \title body -> do
              R.elClass "div" "w-brand-card bg-brand-light-gray box-shadow-brand-card flex flex-col items-center p-4" $ do
                R.elClass "p" "font-medium" $ R.text title
                R.elClass "p" "mt-2 text-center text-brand-sm font-light" $ R.text body
        card "Rich problem editor" "Use our featureful editor to quickly and easily create textbook-quality problems. Features: LaTeX, figures, live preview, variables, automatic solution generation, and more!"
        card "Find new problems" "Having trouble coming up with your own problems? Discover innovative problems created by other teachers in any topic."
        card "Quick problem sets" "No need to spend your valuable time creating new problem sets for each semester. With our collaborative platform, you can find and share problem sets with other teachers."

    R.elClass "div" "mt-20 w-full bg-brand-light-gray pb-20" $ do
      R.elClass "p" "m-1 text-brand-light-gray" $ R.text "PROBLEM EDITOR"
      R.elClass "div" "mt-20 px-10" $ do
        R'.elAttrClass
          "img"
          ("src" =: "/static/problem_editor_screenshot.png")
          "home-screenshot-img mx-auto"
          $ R.blank

    R.elClass "div" "my-10 flex flex-col items-center gap-2 w-full" $ do
      Ob.routeLink (Route.FrontendRoute_Explore :/ Nothing) $ do
        Button.primary "Explore"
      R.elClass "div" "flex" $ do
        R.elClass "div" "flex items-center mr-2" $ do
          R.elClass "hr" "w-20 border-gray-400" R.blank
        R.elClass "p" "" $ R.text "or"
        R.elClass "div" "flex items-center ml-2" $ do
          R.elClass "hr" "w-20 border-gray-400" R.blank
      Ob.routeLink (Route.FrontendRoute_Register :/ ()) $ do
        Button.primary "Create an account"

    R.elClass "div" "p-6 w-full flex border-t border-brand-light-gray" $ do
      R.elClass "p" "" $ R.text "© 2021 Great Problems"

  where
    exampleProblem = mdo
      let problemId = 1

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
        ViewProblem.performCompileRequest onload problemId $ Problem.Compile.Request
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
        ) <- R.elClass "div" "mt-6 mx-10 flex flex-col gap-2" $ mdo
        let problemSummary = \p -> do
              R.elClass "p" "font-medium" $ do
                R.text $ Problem.summary p
        R.dyn_ $ maybe R.blank problemSummary <$> problem
        R.elClass "hr" "w-full border-gray-400" R.blank
        (randomizeVariablesAction, resetVariablesAction) <- do
          R.elClass "div" "flex gap-2" $ do
            randomizeVariables :: R.Event t () <- Button.primarySmallClass'
              "Randomize variables"
              "active:bg-blue-400"
            randomizeVariablesAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
              ViewProblem.performCompileRequest randomizeVariables problemId $ Problem.Compile.Request
                <$> R.constDyn ""
                <*> R.constDyn True
                <*> outputOption
                <*> R.constDyn []
            resetVariables :: R.Event t () <- Button.primarySmallClass'
              "Reset variables"
              "active:bg-blue-400"
            resetVariablesAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
              ViewProblem.performCompileRequest resetVariables problemId $ Problem.Compile.Request
                <$> R.constDyn ""
                <*> R.constDyn False
                <*> outputOption
                <*> R.constDyn []
            return (randomizeVariablesAction', resetVariablesAction')
        (showAnswerAction, showSolutionAction, outputOption) <- do
          R.elClass "div" "flex gap-4" $ do
            R.elClass "p" "font-medium text-brand-primary"
              $ R.text "Show problem with:"
            R.elClass "div" "flex flex-col" $ do
              showAnswer :: R.Dynamic t Bool <- Input.checkboxClass
                "cursor-pointer mr-2 checkbox-brand-primary"
                "font-medium text-brand-primary cursor-pointer"
                "Answer"
              showAnswerAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
                ViewProblem.performCompileRequest (R.updated $ const () <$> showAnswer) problemId $ Problem.Compile.Request
                  <$> R.constDyn ""
                  <*> R.constDyn False
                  <*> outputOption
                  <*> R.constDyn []
              showSolution :: R.Dynamic t Bool <- Input.checkboxClass
                "cursor-pointer mr-2 checkbox-brand-primary"
                "font-medium text-brand-primary cursor-pointer"
                "Solution"
              showSolutionAction' :: R.Dynamic t (Loading.WithLoading (Maybe Compile.Response)) <- do
                ViewProblem.performCompileRequest (R.updated $ const () <$> showSolution) problemId $ Problem.Compile.Request
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

        R.elClass "div" "flex justify-center w-full" $ do
          R.elClass "div" "home-pdf-viewer" $ do
            let showPdf = PdfViewer.widget latestResponse anyLoading (R.constDyn False)
            Util.dynFor latestResponse $ \case
              Nothing -> showPdf
              Just res -> do
                if any (not . T.null) [Compile.resErrorIcemaker res, Compile.resErrorLatex res]
                  then R.text "Something went wrong. Try again later or notify the administrator."
                  else showPdf

        return (randomizeVariablesAction, resetVariablesAction, showAnswerAction, showSolutionAction)

      return ()
