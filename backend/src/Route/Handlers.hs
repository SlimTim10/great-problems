module Route.Handlers
  ( maxRequestBodySize

  -- problems
  , saveProblem
  , duplicateProblem
  , getProblems
  , getProblem
  , deleteProblem

  -- problem sets
  , saveProblemSet
  , getProblemSets
  , getProblemSet
  , addProblemToSet
  , editProblemSet
  , deleteProblemSet

  -- topics
  , createTopic
  , getTopics
  , getTopic
  , updateTopic
  , deleteTopic

  -- users
  , registerUser
  , getUsers
  , getUser
  , updateUser
  , verifyEmail
  , changePassword
  , resetPassword
  , resendEmail
  , signIn
  , signOut

  -- roles
  , getRoles

  -- topic hierarchy
  , getTopicHierarchy

  -- meta settings
  , getMetaSettings
  , getMetaSetting
  , setMetaSetting

  -- compiling
  , compileProblem
  , compileProblemById

  -- figures
  , getFigure
  ) where

import qualified Data.Int as Int
import qualified Data.ByteString as BS
import qualified Data.Word as Word
import qualified Data.Text as T
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as JSON
import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.CaseInsensitive as CI
import qualified System.Directory as Sys
import qualified Snap.Core as Snap
import qualified Snap.Util.FileUploads as Snap

import Common.Lib.Prelude
import qualified Backend.Lib.Util as Util
import qualified Common.Route as Route
import qualified Database.Queries as Queries
import qualified Common.Api.OkResponse as OkResponse
import qualified Common.Api.Error as Error
import qualified Common.Api.Figure as Figure
import qualified Common.Api.User as User
import qualified Common.Api.Problem as Problem
import qualified Common.Api.ProblemStatus as ProblemStatus
import qualified Common.Api.Role as Role
import qualified Common.Api.Compile as Compile
import qualified Common.Api.ProblemSet as ProblemSet
import qualified Common.Api.MetaSetting as MetaSetting
import qualified Common.Api.Topic as Topic
import qualified Common.Api.Request.Register as Register
import qualified Common.Api.Request.ChangePassword as ChangePassword
import qualified Common.Api.Request.ResendEmail as ResendEmail
import qualified Route.Actions as Actions
import qualified Email
import qualified Auth
import qualified Compile

maxRequestBodySize :: Word.Word64
maxRequestBodySize = 2048

maxUploadSize :: Int.Int64
maxUploadSize = 50 * 1024 * 1024 -- 50 MB max file size

-- Create or update problem
saveProblem :: SQL.Connection -> Maybe User.User -> Snap.Snap ()
saveProblem _ Nothing = restrictedResponse
saveProblem conn (Just user) = do
  figures <- fileUploads
  let getTextParam = getTextParamGeneric :: Problem.RequestParam -> Snap.Snap Text
  problemId <- getTextParam Problem.ParamProblemId
  summary <- getTextParam Problem.ParamSummary
  contents <- getTextParam Problem.ParamContents
  topicId <- getTextParam Problem.ParamTopicId
  status <- getTextParam Problem.ParamStatus
  existingAuthor :: Maybe User.User <- IO.liftIO $ do
    if T.null problemId
      then return Nothing
      else
      do
        (Queries.getProblemById conn (read . cs $ problemId)) >>= \case
          Nothing -> return Nothing
          Just p -> return . Just $ Problem.author p
  if
    | isJust existingAuthor && (User.id <$> existingAuthor) /= Just (User.id user)
      -> restrictedResponse
    | T.null contents
      -> Util.writeJSON $ Error.mk "Problem contents cannot be empty"
    | T.null summary
      -> Util.writeJSON $ Error.mk "Summary cannot be empty"
    | (read . cs $ status) == ProblemStatus.Published
      && User.role user `elem` [Role.Contributor, Role.Moderator, Role.Administrator]
      -> do
        -- Published problems must compile without errors
        -- TODO: Error handling on IO exceptions
        -- IO.liftIO
        --   $ Compile.compile
        --   contents
        --   0
        --   figures
        
        if
          -- | (not . T.null $ Compile.p2tErrorProblem2tex r) || (not . T.null $ Compile.p2tErrorLatex r)
          --   -> Util.writeJSON $ Error.mk "Invalid problem. Please check that your problem compiles with no errors before saving."
          | T.null problemId -> do
              Actions.createNewProblem
                conn
                Problem.BareProblem
                { Problem.bpProblemId = Nothing
                , Problem.bpSummary = summary
                , Problem.bpContents = contents
                , Problem.bpTopicId = read . cs $ topicId
                , Problem.bpAuthorId = User.id user
                , Problem.bpStatus = read . cs $ status
                , Problem.bpFigures = figures
                }
          | otherwise -> do
              Actions.updateProblem
                conn
                Problem.BareProblem
                { Problem.bpProblemId = Just $ (read . cs $ problemId :: Integer)
                , Problem.bpSummary = summary
                , Problem.bpContents = contents
                , Problem.bpTopicId = read . cs $ topicId
                , Problem.bpAuthorId = User.id user
                , Problem.bpStatus = read . cs $ status
                , Problem.bpFigures = figures
                }
    | (read . cs $ status) == ProblemStatus.Draft
      && User.role user `elem` [Role.Basic, Role.Contributor, Role.Moderator, Role.Administrator] -> do
        if
          | T.null problemId -> do
              Actions.createNewProblem
                conn
                Problem.BareProblem
                { Problem.bpProblemId = Nothing
                , Problem.bpSummary = summary
                , Problem.bpContents = contents
                , Problem.bpTopicId = read . cs $ topicId
                , Problem.bpAuthorId = User.id user
                , Problem.bpStatus = read . cs $ status
                , Problem.bpFigures = figures
                }
          | otherwise -> do
              Actions.updateProblem
                conn
                Problem.BareProblem
                { Problem.bpProblemId = Just $ (read . cs $ problemId :: Integer)
                , Problem.bpSummary = summary
                , Problem.bpContents = contents
                , Problem.bpTopicId = read . cs $ topicId
                , Problem.bpAuthorId = User.id user
                , Problem.bpStatus = read . cs $ status
                , Problem.bpFigures = figures
                }
      | otherwise -> do
          ambiguousErrorResponse

duplicateProblem :: SQL.Connection -> Maybe User.User -> Integer -> Snap.Snap ()
duplicateProblem conn mUser problemId = do
  mProblem <- IO.liftIO $ Queries.getProblemById conn problemId
  basicDuplicateTopics <-
    IO.liftIO (Queries.getMetaSetting conn MetaSetting.BasicDuplicateTopicIds) >>= \case
    Nothing -> return []
    Just x -> case JSON.decode (cs . MetaSetting.value $ x) :: Maybe [Integer] of
      Nothing -> return []
      Just y -> return y
  let validate :: Maybe (Problem.Problem, User.User) = do
        problem <- mProblem
        user <- mUser
        guard $ User.role user `elem` [Role.Basic, Role.Contributor, Role.Moderator, Role.Administrator]
        guard $ Problem.status problem == ProblemStatus.Published
        guard $
          if User.role user == Role.Basic
          then (Topic.id . Problem.topic $ problem) `elem` basicDuplicateTopics
          else True
        return (problem, user)
  case validate of
    Nothing -> restrictedResponse
    Just (problem, user) -> do
      -- Make duplicate problem as draft with current user as author
      let figures = map (Figure.BareFigure <$> Figure.name <*> Figure.contents) (Problem.figures problem)
      Actions.createNewProblem
        conn
        Problem.BareProblem
        { Problem.bpProblemId = Nothing
        , Problem.bpSummary = Problem.summary problem
        , Problem.bpContents = Problem.contents problem
        , Problem.bpTopicId = Topic.id . Problem.topic $ problem
        , Problem.bpAuthorId = User.id user
        , Problem.bpStatus = ProblemStatus.Draft
        , Problem.bpFigures = figures
        }


getProblems :: SQL.Connection -> Maybe User.User -> Route.Query -> Snap.Snap ()
getProblems conn mUser routeQuery = do
  let problemStatusId :: Maybe Integer =
        fromMaybe (Problem.gpStatus Problem.defaultGetParams)
        $ Route.readParamFromQuery "status" routeQuery
  let routeQuery' = routeQuery <> ("status" =: (cs . show <$> problemStatusId))
  case ProblemStatus.fromId <$> problemStatusId of
    Just ProblemStatus.Published -> do
      Util.writeJSON =<< IO.liftIO (Queries.getProblems conn routeQuery')
    -- Restrict fetching drafts to authorized users (user ID matches draft author ID)
    _ -> case mUser of
      Nothing -> restrictedResponse
      Just user -> do
        let authorId :: Maybe Integer = Route.readParamFromQuery "author" routeQuery'
        if Just (User.id user) /= authorId
          then restrictedResponse
          else Util.writeJSON =<< IO.liftIO (Queries.getProblems conn routeQuery')

getProblem :: SQL.Connection -> Integer -> Snap.Snap ()
getProblem conn problemId = Util.writeJSON =<< IO.liftIO (Queries.getProblemById conn problemId)

deleteProblem :: SQL.Connection -> Maybe User.User -> Integer -> Snap.Snap ()
deleteProblem _ Nothing _ = restrictedResponse
deleteProblem conn (Just user) problemId = do
  IO.liftIO (Queries.getProblemById conn problemId) >>= \case
    Nothing -> ambiguousErrorResponse
    Just problem -> do
      let authorId = User.id . Problem.author $ problem
      if any not
        [ User.role user `elem` [Role.Basic, Role.Contributor, Role.Moderator, Role.Administrator]
        , authorId == User.id user
        ]
        then restrictedResponse
        else Actions.deleteProblem conn problemId

-- Create or update problem set
saveProblemSet :: SQL.Connection -> Maybe User.User -> Snap.Snap ()
saveProblemSet _ Nothing = restrictedResponse
saveProblemSet conn (Just user) = do
  let getTextParam = getTextParamGeneric :: ProblemSet.RequestParam -> Snap.Snap Text
  problemSetId <- getTextParam ProblemSet.ParamProblemSetId
  summary <- getTextParam ProblemSet.ParamSummary
  problemIds <- getTextParam ProblemSet.ParamProblemIds
  existingAuthor :: Maybe User.User <- IO.liftIO $ do
    if T.null problemSetId
      then return Nothing
      else
      do
        (Queries.getProblemSetById conn (read . cs $ problemSetId)) >>= \case
          Nothing -> return Nothing
          Just pset -> return . Just $ ProblemSet.author pset
  if
    | isJust existingAuthor && (User.id <$> existingAuthor) /= Just (User.id user) -> do
      restrictedResponse
    | T.null summary -> do
      Util.writeJSON $ Error.mk "Summary cannot be empty"
    | User.role user `elem` [Role.Basic, Role.Contributor, Role.Moderator, Role.Administrator] -> do
      if
        | T.null problemSetId -> do
          Actions.createNewProblemSet
            conn
            ProblemSet.BareProblemSet
            { ProblemSet.bpsProblemSetId = Nothing
            , ProblemSet.bpsSummary = summary
            , ProblemSet.bpsAuthorId = User.id user
            , ProblemSet.bpsProblemIds = read . cs $ problemIds
            }
        | otherwise -> do
          Actions.updateProblemSet
            conn
            ProblemSet.BareProblemSet
            { ProblemSet.bpsProblemSetId = Just $ (read . cs $ problemSetId :: Integer)
            , ProblemSet.bpsSummary = summary
            , ProblemSet.bpsAuthorId = User.id user
            , ProblemSet.bpsProblemIds = read . cs $ problemIds
            }
    | otherwise -> do
      restrictedResponse

getProblemSets :: SQL.Connection -> Route.Query -> Snap.Snap ()
getProblemSets conn routeQuery = do
  -- No problem set status - no need to restrict access
  Util.writeJSON =<< IO.liftIO (Queries.getProblemSets conn routeQuery)

getProblemSet :: SQL.Connection -> Integer -> Snap.Snap ()
getProblemSet conn problemSetId = do
  Util.writeJSON =<< IO.liftIO (Queries.getProblemSetById conn problemSetId)

addProblemToSet :: SQL.Connection -> Maybe User.User -> Integer -> Snap.Snap ()
addProblemToSet _ Nothing _ = restrictedResponse
addProblemToSet conn (Just user) problemSetId = do
  let getTextParam = getTextParamGeneric :: ProblemSet.RequestParam -> Snap.Snap Text
  mProblemSet :: Maybe ProblemSet.ProblemSet <- IO.liftIO $ Queries.getProblemSetById conn problemSetId
  problemId <- getTextParam ProblemSet.ParamProblemId
  mProblem :: Maybe Problem.Problem <- IO.liftIO $ do
    if T.null problemId
      then return Nothing
      else Queries.getProblemById conn (read . cs $ problemId)
  let existingAuthor :: Maybe User.User = ProblemSet.author <$> mProblemSet
  if
    | isNothing mProblem -> do
        Util.writeJSON $ Error.mk "Problem does not exist"
    | isNothing mProblemSet -> do
        Util.writeJSON $ Error.mk "Problem set does not exist"
    | (User.id <$> existingAuthor) == Just (User.id user) -> do
        let problem = fromJust mProblem
        let problemSet = fromJust mProblemSet
        let problemIds = map Problem.id (ProblemSet.problems problemSet)
        let problemIds' = problemIds <> (pure . Problem.id $ problem)
        Actions.updateProblemSet
          conn
          ProblemSet.BareProblemSet
          { ProblemSet.bpsProblemSetId = Just (ProblemSet.id problemSet)
          , ProblemSet.bpsSummary = ProblemSet.summary problemSet
          , ProblemSet.bpsAuthorId = User.id (ProblemSet.author problemSet)
          , ProblemSet.bpsProblemIds = problemIds'
          }
    | otherwise -> do
        restrictedResponse

editProblemSet :: SQL.Connection -> Maybe User.User -> Integer -> Snap.Snap ()
editProblemSet _ Nothing _ = restrictedResponse
editProblemSet conn (Just user) problemSetId = do
  let getTextParam = getTextParamGeneric :: ProblemSet.RequestParam -> Snap.Snap Text
  mProblemSet :: Maybe ProblemSet.ProblemSet <- IO.liftIO $ Queries.getProblemSetById conn problemSetId
  let existingAuthor :: Maybe User.User = ProblemSet.author <$> mProblemSet
  summary <- getTextParam ProblemSet.ParamSummary
  problemIds <- getTextParam ProblemSet.ParamProblemIds
  if
    | isNothing mProblemSet -> do
        Util.writeJSON $ Error.mk "Problem set does not exist"
    | (User.id <$> existingAuthor) == Just (User.id user) -> do
        Actions.updateProblemSet
          conn
          ProblemSet.BareProblemSet
          { ProblemSet.bpsProblemSetId = Just problemSetId
          , ProblemSet.bpsSummary = summary
          , ProblemSet.bpsAuthorId = User.id user
          , ProblemSet.bpsProblemIds = read . cs $ problemIds
          }
    | otherwise -> do
        restrictedResponse

deleteProblemSet :: SQL.Connection -> Maybe User.User -> Integer -> Snap.Snap ()
deleteProblemSet _ Nothing _ = restrictedResponse
deleteProblemSet conn (Just user) problemSetId = do
  mProblemSet :: Maybe ProblemSet.ProblemSet <- IO.liftIO $ Queries.getProblemSetById conn problemSetId
  let existingAuthor :: Maybe User.User = ProblemSet.author <$> mProblemSet
  if
    | isNothing mProblemSet -> do
        Util.writeJSON $ Error.mk "Problem set does not exist"
    | (User.id <$> existingAuthor) == Just (User.id user) -> do
        Actions.deleteProblemSet conn problemSetId
    | otherwise -> do
        restrictedResponse

createTopic :: SQL.Connection -> Maybe User.User -> Snap.Snap ()
createTopic _ Nothing = restrictedResponse
createTopic conn _ = do
  rawBody <- Snap.readRequestBody maxRequestBodySize
  case JSON.decode rawBody :: Maybe Topic.NewTopic of
    Nothing -> ambiguousErrorResponse
    Just newTopic -> do
      IO.liftIO (Queries.createTopic conn newTopic) >>= \case
        Nothing -> ambiguousErrorResponse
        Just _ -> Util.writeJSON OkResponse.OkResponse

getTopics :: SQL.Connection -> Route.Query -> Snap.Snap ()
getTopics conn routeQuery = do
  topics <- case Route.textParamFromQuery "parent" routeQuery :: Maybe Text of
    Just "null" -> IO.liftIO $ Queries.getRootTopics conn
    Just x -> case readMaybe (cs x) :: Maybe Integer of
      Just parentId -> IO.liftIO $ Queries.getTopicsByParentId conn parentId
      Nothing -> IO.liftIO $ Queries.getTopics conn
    Nothing -> IO.liftIO $ Queries.getTopics conn
  Util.writeJSON topics

getTopic :: SQL.Connection -> Integer -> Snap.Snap ()
getTopic conn topicId = do
  Util.writeJSON =<< IO.liftIO (Queries.getTopicById conn topicId)

updateTopic :: SQL.Connection -> Maybe User.User -> Integer -> Snap.Snap ()
updateTopic _ Nothing _ = restrictedResponse
updateTopic conn _ topicId = do
  rawBody <- Snap.readRequestBody maxRequestBodySize
  case JSON.decode rawBody :: Maybe Topic.Topic of
    Nothing -> ambiguousErrorResponse
    Just topic -> do
      -- Ignore the incoming topic's ID (it should never be changed)
      let topic' = topic { Topic.id = topicId }
      IO.liftIO (Queries.updateTopic conn topic') >>= \case
        Nothing -> ambiguousErrorResponse
        Just _ -> Util.writeJSON OkResponse.OkResponse

-- | Delete a topic. The topic must be empty (no children and no problems).
deleteTopic :: SQL.Connection -> Maybe User.User -> Integer -> Snap.Snap ()
deleteTopic _ Nothing _ = restrictedResponse
deleteTopic conn _ topicId = do
  children <- IO.liftIO (Queries.getTopicsByParentId conn topicId)
  problems <- IO.liftIO (Queries.getProblems conn ("topic" =: Just (cs . show $ topicId)))
  if
    | not . null $ children -> Util.writeJSON $ Error.mk "Topic has children"
    | not . null $ problems -> Util.writeJSON $ Error.mk "Topic has problems"
    | otherwise -> do
        IO.liftIO (Queries.deleteTopic conn topicId)
        Util.writeJSON OkResponse.OkResponse

registerUser :: SQL.Connection -> Snap.Snap ()
registerUser conn = do
  rawBody <- Snap.readRequestBody maxRequestBodySize
  case JSON.decode rawBody :: Maybe Register.Register of
    Nothing -> ambiguousErrorResponse
    Just register -> do
      if any T.null
        [ CI.original $ Register.fullName register
        , CI.original $ Register.email register
        , Register.password register
        ]
        then Util.writeJSON $ Error.mk "Fields must not be empty"
        else
        do
          IO.liftIO (Queries.getUserByEmail conn (Register.email register)) >>= \case
            Just _ -> Util.writeJSON $ Error.mk "Email already registered"
            Nothing -> do
              IO.liftIO (Queries.registerUser conn register) >>= \case
                Nothing -> ambiguousErrorResponse
                Just user -> do
                  secret <- IO.liftIO $ Queries.newEmailVerification conn (User.id user)
                  Email.sendEmailVerification user secret
                  Util.writeJSON OkResponse.OkResponse

getUsers :: SQL.Connection -> Snap.Snap ()
getUsers conn = Util.writeJSON =<< IO.liftIO (Queries.getUsers conn)

getUser :: SQL.Connection -> Maybe User.User -> Integer -> Snap.Snap ()
getUser conn (Just User.User{User.role = Role.Administrator}) userId = do
  Util.writeJSON =<< IO.liftIO (Queries.getUserById conn userId)
getUser _ _ _ = restrictedResponse

updateUser :: SQL.Connection -> Maybe User.User -> Integer -> Snap.Snap ()
updateUser conn (Just User.User{User.role = Role.Administrator}) userId = do
  rawBody <- Snap.readRequestBody maxRequestBodySize
  case JSON.decode rawBody :: Maybe (User.UpdateRequest) of
    Nothing -> ambiguousErrorResponse
    Just req -> do
      IO.liftIO (Queries.updateUserRole conn userId (User.urRole req)) >>= \case
        Nothing -> ambiguousErrorResponse
        Just _ -> Util.writeJSON OkResponse.OkResponse
updateUser _ _ _ = restrictedResponse

verifyEmail :: SQL.Connection -> Text -> Snap.Snap ()
verifyEmail conn secret = do
  verified <- IO.liftIO $ Queries.verifyEmail conn (cs secret)
  if verified
    then Util.writeJSON OkResponse.OkResponse
    else Util.writeJSON $ Error.mk "Invalid email verification link"

changePassword :: SQL.Connection -> Maybe User.User -> Snap.Snap ()
changePassword _ Nothing = restrictedResponse
changePassword conn (Just user) = do
  rawBody <- Snap.readRequestBody maxRequestBodySize
  case JSON.decode rawBody :: Maybe ChangePassword.ChangePassword of
    Nothing -> ambiguousErrorResponse
    Just cp -> do
      case ChangePassword.identification cp of
        ChangePassword.OldPassword oldPassword -> do
          let auth = Auth.Auth (User.email user) oldPassword
          IO.liftIO (Auth.authCheck conn auth) >>= \case
            Auth.Authenticated _ -> changePassword' user cp
            _ -> Util.writeJSON $ Error.mk "Incorrect password"
        ChangePassword.ResetSecret secret -> do
          IO.liftIO (Queries.useResetPassword conn secret) >>= \case
            Nothing -> Util.writeJSON $ Error.mk "Expired or invalid link"
            Just user' -> if
              | user == user' -> changePassword' user cp
              | otherwise -> restrictedResponse
  where
    changePassword' user' cp = do
      if T.null (ChangePassword.newPassword cp)
        then Util.writeJSON $ Error.mk "Password cannot be empty"
        else
        do
          IO.liftIO (Queries.updateUserPassword conn (User.id user') cp) >>= \case
            Nothing -> ambiguousErrorResponse
            Just _ -> Util.writeJSON OkResponse.OkResponse

resetPassword :: SQL.Connection -> Snap.Snap ()
resetPassword conn = do
  rawBody <- Snap.readRequestBody maxRequestBodySize
  case JSON.decode rawBody :: Maybe User.ResetPasswordRequest of
    Nothing -> ambiguousErrorResponse
    Just req -> do
      let email = User.rprEmail req
      IO.liftIO (Queries.getUserByEmail conn email) >>= \case
        -- Don't reveal that the email was not found (for security)
        Nothing -> Util.writeJSON OkResponse.OkResponse
        Just user -> do
          secret <- IO.liftIO $ Queries.newResetPassword conn (User.id user)
          Email.sendResetPasswordEmail user secret
          Util.writeJSON OkResponse.OkResponse

resendEmail :: SQL.Connection -> Snap.Snap ()
resendEmail conn = do
  rawBody <- Snap.readRequestBody maxRequestBodySize
  case JSON.decode rawBody :: Maybe ResendEmail.ResendEmail of
    Nothing -> Util.writeJSON OkResponse.OkResponse
    Just req -> do
      IO.liftIO (Queries.getUserByEmail conn (ResendEmail.email req)) >>= \case
        Nothing -> Util.writeJSON OkResponse.OkResponse
        Just user -> do
          if User.verified user
            then Util.writeJSON OkResponse.OkResponse
            else
            do
              secret <- IO.liftIO $ Queries.newEmailVerification conn (User.id user)
              Email.sendEmailVerification user secret
              Util.writeJSON OkResponse.OkResponse


signIn :: SQL.Connection -> Snap.Snap ()
signIn conn = do
  rawBody <- Snap.readRequestBody maxRequestBodySize
  case JSON.decode rawBody :: Maybe Auth.Auth of
    Nothing -> ambiguousErrorResponse
    Just auth -> do
      IO.liftIO (Auth.authCheck conn auth) >>= \case
        Auth.Indefinite -> Util.writeJSON $ Error.mk "Incorrect email or password. Please try again."
        Auth.Unverified _ -> Util.writeJSON $ Error.mk "Account not verified. Please check your email to complete the verification process."
        Auth.Authenticated user -> do
          session <- IO.liftIO $ Auth.newSession conn user
          Util.addCookie "sessionId" session
          Util.addCookie "user" (cs $ JSON.encode user)
          Util.writeJSON OkResponse.OkResponse

signOut :: SQL.Connection -> Maybe Auth.Session -> Snap.Snap ()
signOut conn mSession = do
  IO.liftIO $ mapM_ (Auth.removeSession conn) mSession
  Util.removeCookie "sessionId"
  Util.removeCookie "user"
  Util.writeJSON OkResponse.OkResponse

getRoles :: SQL.Connection -> Snap.Snap ()
getRoles conn = Util.writeJSON =<< IO.liftIO (Queries.getRoles conn)

getTopicHierarchy :: SQL.Connection -> Integer -> Snap.Snap ()
getTopicHierarchy conn topicId = do
  IO.liftIO (Queries.getTopicById conn topicId) >>= \case
    Nothing -> Util.writeJSON $ Error.mk "Topic not found"
    Just topic -> Util.writeJSON =<< IO.liftIO (Queries.getTopicHierarchy conn topic)

-- | Get all meta settings. Only administrators are authorized to do this.
getMetaSettings :: SQL.Connection -> Maybe User.User -> Snap.Snap ()
getMetaSettings conn (Just User.User{User.role = Role.Administrator}) = do
  Util.writeJSON =<< IO.liftIO (Queries.getMetaSettings conn)
getMetaSettings _ _ = restrictedResponse

getMetaSetting :: SQL.Connection -> MetaSetting.Setting -> Snap.Snap ()
getMetaSetting conn setting = Util.writeJSON =<< IO.liftIO (Queries.getMetaSetting conn setting)

-- | Set a meta setting provided in the request body. Only administrators are authorized to do this.
setMetaSetting :: SQL.Connection -> Maybe User.User -> Snap.Snap ()
setMetaSetting conn (Just User.User{User.role = Role.Administrator}) = do
  rawBody <- Snap.readRequestBody maxRequestBodySize
  case JSON.decode rawBody :: Maybe MetaSetting.MetaSetting of
    Nothing -> ambiguousErrorResponse
    Just metaSetting -> do
      IO.liftIO (Queries.setMetaSetting conn metaSetting) >>= \case
        Nothing -> ambiguousErrorResponse
        Just _ -> Util.writeJSON OkResponse.OkResponse
setMetaSetting _ _ = restrictedResponse

compileProblem :: Snap.Snap (Either Error.Error Text)
compileProblem = do
  figures :: [Figure.BareFigure] <- fileUploads
  let getTextParam = getTextParamGeneric :: Compile.RequestParam -> Snap.Snap Text
  contents <- getTextParam Compile.ParamContents
  randomizeVariables <- getTextParam Compile.ParamRandomizeVariables
  let randomSeed = readMaybe (cs randomizeVariables) :: Maybe Compile.RandomSeed
  if
    | T.null contents
      -> return $ Left $ Error.mk "Problem contents cannot be empty"
    | isNothing randomSeed
      -> return $ Left $ Error.mk "Invalid random seed"
    | otherwise
      -> IO.liftIO $ Compile.compile contents (fromJust randomSeed) figures

compileProblemById :: SQL.Connection -> Integer -> Snap.Snap (Either Error.Error Text)
compileProblemById conn problemId = do
  -- Need this to prepare POST parameters
  void $ Snap.handleMultipart
    Snap.defaultUploadPolicy
    (const . const $ return ())

  let getTextParam = getTextParamGeneric :: Compile.RequestParam -> Snap.Snap Text
  randomizeVariables <- getTextParam Compile.ParamRandomizeVariables
  let randomSeed = readMaybe (cs randomizeVariables) :: Maybe Compile.RandomSeed
  IO.liftIO (Queries.getProblemById conn problemId) >>= \case
    Nothing -> return $ Left $ Error.mk "Problem does not exist"
    Just problem -> do
      let figures :: [Figure.BareFigure] = Problem.figures problem <&> \figure ->
            Figure.BareFigure
            { Figure.bfName = Figure.name figure
            , Figure.bfContents = Figure.contents figure
            }
      case randomSeed of
        Nothing -> return $ Left $ Error.mk "Invalid random seed"
        Just randomSeed' -> IO.liftIO
          $ Compile.compile (Problem.contents problem) randomSeed' figures

getFigure :: SQL.Connection -> Integer -> Snap.Snap ()
getFigure conn figureId = do
  IO.liftIO (Queries.getFigureById conn figureId) >>= \case
    Nothing -> Util.writeJSON $ Error.mk "Figure does not exist"
    Just figure -> do
      Snap.modifyResponse $ Snap.setHeader "Content-Type" "application/octet-stream"
      -- For browsers
      Snap.modifyResponse $ Snap.setHeader "Content-Disposition" $
        "attachment; filename=\"" <> cs (Figure.name figure) <> "\""
      -- For frontend easier parsing
      Snap.modifyResponse $ Snap.setHeader "Filename" $ cs (Figure.name figure)
      Snap.writeBS $ Figure.contents figure


-- | Get the files from the POST request and store them in memory.
-- Returns a list of the files.
-- This must be called before getting other POST parameters from the request.
fileUploads :: Snap.Snap [Figure.BareFigure]
fileUploads = do
  tmpDir <- IO.liftIO Sys.getTemporaryDirectory
  let handleFile = \info -> \case
        Left e -> do
          print e
          return Nothing
        Right fp -> case Snap.partFileName info of
          Nothing -> return Nothing
          Just fName -> do
            contents <- BS.readFile fp
            return . Just $ Figure.BareFigure (cs fName) contents
  figures <- Snap.handleFileUploads
    tmpDir
    Snap.defaultUploadPolicy
    (const $ Snap.allowWithMaximumSize maxUploadSize)
    handleFile
    <&> catMaybes
  return figures

-- | Not meant to be used directly.
-- Define a local function with specific type.
-- e.g. let getTextParam = getTextParamGeneric :: Problem.RequestParam -> Snap.Snap Text
getTextParamGeneric :: Show a => a -> Snap.Snap Text
getTextParamGeneric param = do
  Snap.rqPostParam (cs . show $ param)
    <$> Snap.getRequest
    <&> cs . BS.concat . fromMaybe mempty

restrictedResponse :: Snap.Snap ()
restrictedResponse = Util.writeJSON $ Error.mk "No access"

ambiguousErrorResponse :: Snap.Snap ()
ambiguousErrorResponse = Util.writeJSON $ Error.mk "Something went wrong"
