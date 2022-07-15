module Route.Actions where

import qualified Data.ByteString.Lazy as LBS
import qualified Control.Monad.IO.Class as IO
import qualified Database.PostgreSQL.Simple as SQL
import qualified Network.Wreq as Wreq
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as HTTP
import qualified Snap.Core as Snap

import Common.Lib.Prelude
import qualified Common.Api.Figure as Figure
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Problem as Problem
import qualified Database.Queries as Queries
import qualified Common.Api.Error as Error
import qualified Common.Api.ProblemSet as ProblemSet
import qualified Common.Api.OkResponse as OkResponse
import qualified Backend.Lib.Util as Util

createNewProblem :: SQL.Connection -> Problem.BareProblem -> Snap.Snap ()
createNewProblem conn problem = do
  IO.liftIO (Queries.createProblem conn problem) >>= \case
    Nothing -> Util.writeJSON $ Error.mk "Something went wrong"
    Just createdProblem -> Util.writeJSON createdProblem
        
createNewProblemSet :: SQL.Connection -> ProblemSet.BareProblemSet -> Snap.Snap ()
createNewProblemSet conn problemSet = do
  IO.liftIO (Queries.createProblemSet conn problemSet) >>= \case
    Nothing -> Util.writeJSON $ Error.mk "Something went wrong"
    Just createdProblemSet -> Util.writeJSON createdProblemSet

deleteProblemSet :: SQL.Connection -> Integer -> Snap.Snap ()
deleteProblemSet conn problemSetId = do
  IO.liftIO $ Queries.deleteProblemSet conn problemSetId
  Util.writeJSON OkResponse.OkResponse

updateProblem :: SQL.Connection -> Problem.BareProblem -> Snap.Snap ()
updateProblem conn problem = do
  IO.liftIO (Queries.updateProblem conn problem) >>= \case
    Nothing -> Util.writeJSON $ Error.mk "Something went wrong"
    Just updatedProblem -> Util.writeJSON updatedProblem

updateProblemSet :: SQL.Connection -> ProblemSet.BareProblemSet -> Snap.Snap ()
updateProblemSet conn problemSet = do
  IO.liftIO (Queries.updateProblemSet conn problemSet) >>= \case
    Nothing -> Util.writeJSON $ Error.mk "Something went wrong"
    Just updatedProblemSet -> Util.writeJSON updatedProblemSet

deleteProblem :: SQL.Connection -> Integer -> Snap.Snap ()
deleteProblem conn problemId = do
  IO.liftIO $ Queries.deleteProblem conn problemId
  Util.writeJSON OkResponse.OkResponse

requestProblem2texCompileProblem
  :: IO.MonadIO m
  => Text -- ^ Problem contents
  -> Maybe Text -- Randomize variables
  -> Maybe Text -- Output option
  -> [Figure.BareFigure] -- Files
  -> m LBS.ByteString
requestProblem2texCompileProblem contents mRandomizeVariables mOutputOption figures = do
  let randomizeVariables = fromMaybe "false" mRandomizeVariables
  let outputOption = fromMaybe (cs . show $ Compile.QuestionOnly) mOutputOption
  let figureToPart = \figure -> HTTP.partFileRequestBody
        "multiplefiles"
        (cs $ Figure.bfName figure)
        $ HTTP.RequestBodyBS (Figure.bfContents figure)
  response <- IO.liftIO $ Wreq.post
    "https://icewire.ca/uploadprb"
    $ [ Wreq.partText "prbText" contents
      , Wreq.partText "prbName" "tmp"
      , Wreq.partText "random" randomizeVariables
      , Wreq.partText "outFlag" outputOption
      , Wreq.partText "submit1" "putDatabase"
      ] ++ map figureToPart figures
  return $ response ^. Wreq.responseBody

