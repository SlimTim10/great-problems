module Home
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Control.Monad.IO.Class as IO
import qualified Data.Time.Clock as Time
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Obelisk.Generated.Static as Ob
import qualified MyReflex.Dom.Widget.Basic as R'
import qualified Reflex.Dom.Core as R

import qualified Common.Route as Route
import qualified Common.Api.Problem as Problem
import qualified Common.Api.Compile as Compile
import qualified Common.Api.MetaSetting as MetaSetting
import qualified Widget.Button as Button
import qualified Widget.Input as Input
import qualified Problem.Loading as Loading
import qualified Problem.Compile
import qualified Problem.PdfViewer as PdfViewer

widget
  :: forall t m js.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , MonadFix m
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
          ("src" =: Ob.static @"problem_editor_screenshot.png")
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
      let fallbackProblemId = 1
      mSetting :: R.Event t (Maybe MetaSetting.MetaSetting) <- Util.getOnload
        $ Route.apiHref $ Route.Api_MetaSettings :/ (Just MetaSetting.ExampleProblemId)
      let problemId' :: R.Event t Integer = mSetting <&> \case
            Nothing -> fallbackProblemId
            Just x -> read . cs $ MetaSetting.value x

      problem :: R.Dynamic t (Maybe Problem.Problem) <- do
        let url :: R.Event t Text = R.ffor problemId' $ \pid -> do
              Route.apiHref $ Route.Api_Problems :/
                (Just pid, Problem.getParamsToRouteQuery Problem.getParamsDefault)
        r :: R.Event t (Maybe Problem.Problem) <- R.getAndDecode url
        R.holdDyn Nothing r

      let compileProblem = \req -> do
            problemId'' <- R.holdDyn fallbackProblemId problemId'
            res :: R.Event t (R.Dynamic t (Loading.WithLoading Problem.Compile.Response)) <-
              R.dyn . R.ffor problemId'' $ \pid -> Problem.Compile.performRequestWithId pid req
            res' :: R.Event t (Loading.WithLoading Problem.Compile.Response) <-
              R.switchHold R.never (R.updated <$> res)
            ct <- IO.liftIO Time.getCurrentTime
            R.holdDyn (Loading.WithLoading (Problem.Compile.Response ct Nothing) True) res'

      onloadAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
        req <- Problem.Compile.mkRequest (const () <$> problemId')
          (R.constDyn "")
          (R.constDyn Problem.Compile.NoChange)
          (R.constDyn Compile.QuestionOnly)
          (R.constDyn [])
        compileProblem req
      
      let actions =
            [ onloadAction
            , randomizeVariablesAction
            , resetVariablesAction
            , showAnswerAction
            , showSolutionAction
            ] :: [R.Dynamic t (Loading.WithLoading Problem.Compile.Response)]
            
      currentResponse :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
        -- The current response should have the latest request time
        let switchToLatest = \new old -> do
              let tNew = Problem.Compile.reqTime . Loading.action $ new
              let tOld = Problem.Compile.reqTime . Loading.action $ old
              if tNew >= tOld
                then Just new
                else Nothing
        t <- IO.liftIO Time.getCurrentTime
        R.foldDynMaybe
          switchToLatest
          (Loading.WithLoading (Problem.Compile.Response t Nothing) False)
          (R.leftmost . map R.updated $ actions)
        
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
          R.elClass "div" "flex gap-2" $ mdo
            randomizeVariables :: R.Event t () <- Button.primarySmallClass'
              "Randomize variables"
              "active:bg-blue-400"
            randomizeVariablesAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
              req <- Problem.Compile.mkRequest randomizeVariables
                (R.constDyn "")
                (R.constDyn Problem.Compile.Randomize)
                outputOption
                (R.constDyn [])
              compileProblem req
            resetVariables :: R.Event t () <- Button.primarySmallClass'
              "Reset variables"
              "active:bg-blue-400"
            resetVariablesAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
              req <- Problem.Compile.mkRequest resetVariables
                (R.constDyn "")
                (R.constDyn Problem.Compile.Reset)
                outputOption
                (R.constDyn [])
              compileProblem req
            return (randomizeVariablesAction, resetVariablesAction)
        (showAnswerAction, showSolutionAction, outputOption) <- do
          R.elClass "div" "flex gap-4" $ do
            R.elClass "p" "font-medium text-brand-primary"
              $ R.text "Show problem with:"
            R.elClass "div" "flex flex-col" $ mdo
              showAnswer :: R.Dynamic t Bool <- Input.checkboxClass
                "cursor-pointer mr-2 checkbox-brand-primary"
                "font-medium text-brand-primary cursor-pointer"
                "Answer"
              showAnswerAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
                req <- Problem.Compile.mkRequest (R.updated $ const () <$> showAnswer)
                  (R.constDyn "")
                  (R.constDyn Problem.Compile.NoChange)
                  outputOption
                  (R.constDyn [])
                compileProblem req
              showSolution :: R.Dynamic t Bool <- Input.checkboxClass
                "cursor-pointer mr-2 checkbox-brand-primary"
                "font-medium text-brand-primary cursor-pointer"
                "Solution"
              showSolutionAction :: R.Dynamic t (Loading.WithLoading Problem.Compile.Response) <- do
                req <- Problem.Compile.mkRequest (R.updated $ const () <$> showSolution)
                  (R.constDyn "")
                  (R.constDyn Problem.Compile.NoChange)
                  outputOption
                  (R.constDyn [])
                compileProblem req
              let outputOption' :: R.Dynamic t Compile.OutputOption =
                    (\showAnswer' showSolution' ->
                       case (showAnswer', showSolution') of
                         (False, False) -> Compile.QuestionOnly
                         (False, True) -> Compile.WithSolution
                         (True, False) -> Compile.WithAnswer
                         (True, True) -> Compile.WithSolutionAndAnswer
                    ) <$> showAnswer <*> showSolution
              return (showAnswerAction, showSolutionAction, outputOption')

        R.elClass "div" "flex justify-center w-full" $ do
          R.elClass "div" "home-pdf-viewer" $ do
            PdfViewer.widget
              ((Problem.Compile.response . Loading.action) <$> currentResponse)
              (Loading.loading <$> currentResponse)
              (R.constDyn False)

        return (randomizeVariablesAction, resetVariablesAction, showAnswerAction, showSolutionAction)

      return ()
