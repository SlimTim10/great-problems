{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Backend where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Word as Word
import qualified Data.CaseInsensitive as CI
import qualified Database.PostgreSQL.Simple as SQL
import qualified Network.Wreq as Wreq
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as HTTP
import qualified Snap.Core as Snap
import qualified Snap.Util.FileUploads as Snap
import qualified System.Directory as Sys
import qualified Obelisk.Backend as Ob
import Obelisk.Route ( pattern (:/) )

import qualified Common.Route as Route
import qualified Common.Api.Error as Error
import qualified Database
import qualified Database.Queries as Queries
import qualified Common.Api.User as User
import qualified Common.Api.Register as Register
import qualified Common.Api.Role as Role
import qualified Common.Api.OkResponse as OkResponse
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Problem as Problem
import qualified Common.Api.Figure as Figure
import qualified Auth
import qualified Email
import Global

maxRequestBodySize :: Word.Word64
maxRequestBodySize = 2048

backend :: Ob.Backend Route.BackendRoute Route.FrontendRoute
backend = Ob.Backend
  { Ob._backend_run = \serve -> do
      -- Connect to the database
      conn <- Database.connect

      serve $ \case
        Route.BackendRoute_Missing :/ () -> return ()
        Route.BackendRoute_Api :/ apiRoute -> do
          mSession :: Maybe Auth.Session <- maybe
            Nothing
            (pure . cs . Snap.cookieValue)
            <$> Snap.getCookie "sessionId"
          mUser :: Maybe User.User <- IO.liftIO $ maybe (pure Nothing) (Auth.getUser conn) mSession
          when (isNothing mUser) $ do
            removeCookie "sessionId"
            removeCookie "user"
          case apiRoute of
            Route.Api_Problems :/ (Nothing, query) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> writeJSON =<< IO.liftIO (Queries.getProblems conn Nothing query)
                Snap.POST -> case mUser of
                  Nothing -> writeJSON $ Error.mk "No access"
                  Just user -> handleSaveProblem conn user
                _ -> return () -- TODO: implement put, delete
            Route.Api_Problems :/ (Just problemId, query) -> do
              writeJSON =<< IO.liftIO (Queries.getProblemById conn problemId query)
            Route.Api_Topics :/ query -> do
              topics <- case fromMaybe Nothing (Map.lookup "parent" query) of
                Just "null" -> IO.liftIO $ Queries.getRootTopics conn
                Just x -> case readMaybe (cs x) :: Maybe Integer of
                  Just parentId -> IO.liftIO $ Queries.getTopicsByParentId conn parentId
                  Nothing -> IO.liftIO $ Queries.getTopics conn
                Nothing -> IO.liftIO $ Queries.getTopics conn
              writeJSON topics
            Route.Api_Users :/ Nothing -> do
              writeJSON =<< IO.liftIO (Queries.getUsers conn)
            Route.Api_Users :/ (Just userId) -> do
              writeJSON =<< IO.liftIO (Queries.getUserById conn userId)
            Route.Api_TopicHierarchy :/ Nothing -> do
              writeJSON $ Error.mk "Not yet implemented"
            Route.Api_TopicHierarchy :/ Just topicId -> do
              IO.liftIO (Queries.getTopicById conn topicId) >>= \case
                Nothing -> writeJSON $ Error.mk "Topic not found"
                Just topic -> writeJSON =<< IO.liftIO (Queries.getTopicHierarchy conn topic)
            Route.Api_Register :/ () -> do
              rawBody <- Snap.readRequestBody maxRequestBodySize
              case JSON.decode rawBody :: Maybe Register.Register of
                Nothing -> writeJSON $ Error.mk "Something went wrong"
                Just register -> do
                  if any T.null
                    [ CI.original $ Register.fullName register
                    , CI.original $ Register.email register
                    , Register.password register
                    ]
                    then writeJSON $ Error.mk "Fields must not be empty"
                    else do
                    IO.liftIO (Queries.getUserByEmail conn (Register.email register)) >>= \case
                      Just _ -> writeJSON $ Error.mk "Email already registered"
                      Nothing -> do
                        IO.liftIO (Queries.registerUser conn register) >>= \case
                          Nothing -> writeJSON $ Error.mk "Something went wrong"
                          Just user -> do
                            secret <- IO.liftIO $ Queries.newEmailVerification conn (User.id user)
                            Email.sendEmailVerification user secret
                            writeJSON OkResponse.OkResponse
            Route.Api_VerifyEmail :/ secret -> do
              verify <- IO.liftIO $ Queries.verifyEmail conn (cs secret)
              if verify
                then writeJSON OkResponse.OkResponse
                else writeJSON $ Error.mk "Invalid email verification code"
            Route.Api_SignIn :/ () -> do
              rawBody <- Snap.readRequestBody maxRequestBodySize
              case JSON.decode rawBody :: Maybe Auth.Auth of
                Nothing -> writeJSON $ Error.mk "Something went wrong"
                Just auth -> do
                  IO.liftIO (Auth.authCheck conn auth) >>= \case
                    Auth.Indefinite -> writeJSON $ Error.mk "Incorrect email or password. Please try again."
                    Auth.Unverified _ -> writeJSON $ Error.mk "Account not verified. Please check your email to complete the verification process."
                    Auth.Authenticated user -> do
                      session <- IO.liftIO $ Auth.newSession conn user
                      addCookie "sessionId" session
                      addCookie "user" (cs $ JSON.encode user)
                      writeJSON OkResponse.OkResponse
            Route.Api_SignOut :/ () -> do
              IO.liftIO $ mapM_ (Auth.removeSession conn) mSession
              removeCookie "sessionId"
              removeCookie "user"
            Route.Api_Compile :/ Nothing -> Snap.rqMethod <$> Snap.getRequest >>= \case
              Snap.POST -> case mUser of
                Nothing -> writeJSON $ Error.mk "No access"
                Just _ -> handleCompileProblem
              _ -> return ()
            Route.Api_Compile :/ Just problemId -> Snap.rqMethod <$> Snap.getRequest >>= \case
              Snap.POST -> handleCompileProblemById conn problemId
              _ -> return ()
            Route.Api_Figures :/ figureId -> do
              IO.liftIO (Queries.getFigureById conn figureId) >>= \case
                Nothing -> writeJSON $ Error.mk "Figure does not exist"
                Just figure -> do
                  Snap.modifyResponse $ Snap.setHeader "Content-Type" "application/octet-stream"
                  -- For browsers
                  Snap.modifyResponse $ Snap.setHeader "Content-Disposition" $
                    "attachment; filename=\"" <> cs (Figure.name figure) <> "\""
                  -- For frontend easier parsing
                  Snap.modifyResponse $ Snap.setHeader "Filename" $ cs (Figure.name figure)
                  Snap.writeBS $ Figure.contents figure
  , Ob._backend_routeEncoder = Route.fullRouteEncoder
  }

-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (Snap.MonadSnap m, JSON.ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  Snap.writeLBS . JSON.encode $ a

-- | Mark response as 'application/json'
jsonResponse :: Snap.MonadSnap m => m ()
jsonResponse = Snap.modifyResponse $
  Snap.setHeader "Content-Type" "application/json"

mkCookie
  :: Text -- ^ name
  -> Text -- ^ value
  -> Snap.Cookie
mkCookie name value = Snap.Cookie
  (cs name :: BS.ByteString)
  (cs value :: BS.ByteString)
  Nothing
  Nothing
  (Just "/")
  False
  False

addCookie
  :: Snap.MonadSnap m
  => Text -- ^ cookie name
  -> Text -- ^ cookie value
  -> m ()
addCookie name value =
  Snap.modifyResponse
  $ Snap.addResponseCookie
  $ mkCookie name value

removeCookie
  :: Snap.MonadSnap m
  => Text -- ^ cookie name
  -> m ()
removeCookie name = Snap.expireCookie $ mkCookie name ""

-- Create or update problem
handleSaveProblem :: SQL.Connection -> User.User -> Snap.Snap ()
handleSaveProblem conn user = do
  figures <- handleFileUploads
  let getTextParam :: Problem.RequestParam -> Snap.Snap Text = \param -> do
        Snap.rqPostParam (cs . show $ param)
          <$> Snap.getRequest
          <&> cs . BS.concat . fromMaybe mempty
  problemId <- getTextParam Problem.ParamProblemId
  summary <- getTextParam Problem.ParamSummary
  contents <- getTextParam Problem.ParamContents
  topicId <- getTextParam Problem.ParamTopicId
  authorId <- getTextParam Problem.ParamAuthorId
  authorIdFromDb :: Maybe Integer <- IO.liftIO $ do
    if T.null problemId
      then return Nothing
      else do
      (Queries.getProblemById conn (read . cs $ problemId) mempty) >>= \case
        Nothing -> return Nothing
        Just p -> return . Just $ either id User.id (Problem.author p)
  if
    | any not
      [ T.null problemId || (read . cs $ authorId) == User.id user
      , T.null problemId || authorIdFromDb == Just (User.id user)
      , User.role user == Role.Contributor || User.role user == Role.Moderator
      ]
      -> writeJSON $ Error.mk "No access"
    | T.null contents
      -> writeJSON $ Error.mk "Problem contents cannot be empty"
    | T.null summary
      -> writeJSON $ Error.mk "Summary cannot be empty"
    | otherwise -> do
        response :: LBS.ByteString <- requestIcemakerCompileProblem
          contents
          Nothing
          Nothing
          figures
        case JSON.decode response :: Maybe Compile.IcemakerResponse of
          Nothing -> writeJSON $ Error.mk "Something went wrong"
          Just r -> if
            | (not . T.null $ Compile.iceErrorIcemaker r) || (not . T.null $ Compile.iceErrorLatex r)
              -> writeJSON $ Error.mk "Invalid problem. Please check that your problem compiles with no errors before saving."
            -- Creating a new problem
            | T.null problemId -> do
                let newProblem = Problem.BareProblem
                      { Problem.bpProblemId = Nothing
                      , Problem.bpSummary = summary
                      , Problem.bpContents = contents
                      , Problem.bpTopicId = read . cs $ topicId
                      , Problem.bpAuthorId = read . cs $ authorId
                      , Problem.bpFigures = figures
                      }
                IO.liftIO (Queries.createProblem conn newProblem) >>= \case
                  Nothing -> writeJSON $ Error.mk "Something went wrong"
                  Just createdProblem -> do
                    writeJSON createdProblem
            -- Updating an existing problem
            | otherwise -> do
                let updateProblem = Problem.BareProblem
                      { Problem.bpProblemId = Just $ (read . cs $ problemId :: Integer)
                      , Problem.bpSummary = summary
                      , Problem.bpContents = contents
                      , Problem.bpTopicId = read . cs $ topicId
                      , Problem.bpAuthorId = read . cs $ authorId
                      , Problem.bpFigures = figures
                      }
                IO.liftIO (Queries.updateProblem conn updateProblem) >>= \case
                  Nothing -> writeJSON $ Error.mk "Something went wrong"
                  Just updatedProblem -> do
                    writeJSON updatedProblem

requestIcemakerCompileProblem
  :: IO.MonadIO m
  => Text -- ^ Problem contents
  -> Maybe Text -- Randomize variables
  -> Maybe Text -- Output option
  -> [Figure.BareFigure] -- Files
  -> m LBS.ByteString
requestIcemakerCompileProblem contents mRandomizeVariables mOutputOption figures = do
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

handleCompileProblem :: Snap.Snap ()
handleCompileProblem = do
  figures <- handleFileUploads
  let getTextParam :: Compile.RequestParam -> Snap.Snap Text = \param -> do
        Snap.rqPostParam (cs . show $ param)
          <$> Snap.getRequest
          <&> cs . BS.concat . fromMaybe mempty
  contents <- getTextParam Compile.ParamContents
  randomizeVariables <- getTextParam Compile.ParamRandomizeVariables
  outputOption <- getTextParam Compile.ParamOutputOption
  if
    | T.null contents
      -> writeJSON $ Error.mk "Problem contents cannot be empty"
    | otherwise -> do
        response :: LBS.ByteString <- requestIcemakerCompileProblem
          contents
          (Just randomizeVariables)
          (Just outputOption)
          figures
        case JSON.decode response :: Maybe Compile.IcemakerResponse of
          Nothing -> writeJSON $ Error.mk "Something went wrong"
          Just r -> writeJSON Compile.Response
            { Compile.resErrorIcemaker = Compile.iceErrorIcemaker r
            , Compile.resErrorLatex = Compile.iceErrorLatex r
            , Compile.resPdfContents = Compile.icePdfContents r
            , Compile.resTerminalOutput = Compile.iceTerminalOutput r
            }

handleCompileProblemById :: SQL.Connection -> Integer -> Snap.Snap ()
handleCompileProblemById conn problemId = do
  -- Need this to prepare POST parameters
  void $ Snap.handleMultipart
    Snap.defaultUploadPolicy
    (const . const $ return ())
  let getTextParam :: Compile.RequestParam -> Snap.Snap Text = \param -> do
        Snap.rqPostParam (cs . show $ param)
          <$> Snap.getRequest
          <&> cs . BS.concat . fromMaybe mempty
  randomizeVariables <- fmap (mfilter (not . T.null) . Just)
    $ getTextParam Compile.ParamRandomizeVariables
  outputOption <- fmap (mfilter (not . T.null) . Just)
    $ getTextParam Compile.ParamOutputOption
  IO.liftIO (Queries.getProblemById conn problemId mempty) >>= \case
    Nothing -> writeJSON $ Error.mk "Problem does not exist"
    Just problem -> do
      let figures = Problem.figures problem <&> \figure ->
            Figure.BareFigure
            { Figure.bfName = Figure.name figure
            , Figure.bfContents = Figure.contents figure
            }
      response :: LBS.ByteString <- requestIcemakerCompileProblem
        (Problem.contents problem)
        randomizeVariables
        outputOption
        figures
      case JSON.decode response :: Maybe Compile.IcemakerResponse of
        Nothing -> writeJSON $ Error.mk "Something went wrong"
        Just r -> writeJSON Compile.Response
          { Compile.resErrorIcemaker = Compile.iceErrorIcemaker r
          , Compile.resErrorLatex = Compile.iceErrorLatex r
          , Compile.resPdfContents = Compile.icePdfContents r
          , Compile.resTerminalOutput = Compile.iceTerminalOutput r
          }

-- | Get the files from the POST request and store them in memory.
-- Returns a list of the files.
-- This must be called before getting other POST parameters from the request.
handleFileUploads :: Snap.Snap [Figure.BareFigure]
handleFileUploads = do
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
    (const $ Snap.allowWithMaximumSize 20000) -- 20 kB max file size
    handleFile
    <&> catMaybes
  return figures
