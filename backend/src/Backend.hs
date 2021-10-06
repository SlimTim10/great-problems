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
import qualified Snap.Core as Snap
import qualified Snap.Util.FileUploads as Snap
import qualified System.Directory as Dir
import System.FilePath ((</>))
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
  fileUploads <- handleFileUploads
  let getTextParam :: Problem.RequestParam -> Snap.Snap Text = \param -> do
        Snap.rqPostParam (cs . show $ param)
          <$> Snap.getRequest
          <&> cs . BS.concat . fromMaybe mempty
  problemId <- getTextParam Problem.ParamProblemId
  summary <- getTextParam Problem.ParamSummary
  content <- getTextParam Problem.ParamContent
  topicId <- getTextParam Problem.ParamTopicId
  authorId <- getTextParam Problem.ParamAuthorId
  if
    | (read . cs $ authorId) /= User.id user
      || not (User.role user == Role.Contributor || User.role user == Role.Moderator)
      -> writeJSON $ Error.mk "No access"
    | T.null content
      -> writeJSON $ Error.mk "Problem content cannot be empty"
    | T.null summary
      -> writeJSON $ Error.mk "Summary cannot be empty"
    | otherwise -> do
        response :: LBS.ByteString <- requestIcemakerCompileProblem
          content
          Nothing
          Nothing
          fileUploads
        case JSON.decode response :: Maybe Compile.IcemakerResponse of
          Nothing -> writeJSON $ Error.mk "Something went wrong"
          Just r -> if
            | (not . T.null $ Compile.errorIcemaker r) || (not . T.null $ Compile.errorLatex r)
              -> writeJSON $ Error.mk "Invalid problem. Please check that your problem compiles with no errors before saving."
            | T.null problemId -> do
                let newProblem = Problem.CreateProblem
                      { Problem.cpSummary = summary
                      , Problem.cpContent = content
                      , Problem.cpTopicId = read . cs $ topicId
                      , Problem.cpAuthorId = read . cs $ authorId
                      }
                -- TODO: save problem figures too
                writeJSON =<< IO.liftIO (Queries.createProblem conn newProblem)
            | otherwise -> do
                let updateProblem = Problem.UpdateProblem
                      { Problem.upProblemId = read . cs $ problemId
                      , Problem.upSummary = summary
                      , Problem.upContent = content
                      , Problem.upTopicId = read . cs $ topicId
                      , Problem.upAuthorId = read . cs $ authorId
                      }
                -- TODO: save problem figures too
                writeJSON =<< IO.liftIO (Queries.updateProblem conn updateProblem)
  -- Delete the uploaded files from the server
  IO.liftIO $ forM_ (map filePath fileUploads) $ \fp -> Dir.removeFile fp

requestIcemakerCompileProblem
  :: IO.MonadIO m
  => Text -- ^ Problem content
  -> Maybe Text -- Randomize variables
  -> Maybe Text -- Output option
  -> [FileUpload] -- Files
  -> m LBS.ByteString
requestIcemakerCompileProblem content mRandomizeVariables mOutputOption fileUploads = do
  IO.liftIO $ print content -- DEBUG
  IO.liftIO $ print mRandomizeVariables -- DEBUG
  IO.liftIO $ print mOutputOption -- DEBUG
  IO.liftIO $ print (length fileUploads) -- DEBUG
  let randomizeVariables = fromMaybe "false" mRandomizeVariables
  let outputOption = fromMaybe (cs . show $ Compile.QuestionOnly) mOutputOption
  response <- IO.liftIO $ Wreq.post
    "https://icewire.ca/uploadprb"
    $ [ Wreq.partText "prbText" content
      , Wreq.partText "prbName" "tmp"
      , Wreq.partText "random" randomizeVariables
      , Wreq.partText "outFlag" outputOption
      , Wreq.partText "submit1" "putDatabase"
      ] ++ map filePart fileUploads
  return $ response ^. Wreq.responseBody

handleCompileProblem :: Snap.Snap ()
handleCompileProblem = do
  fileUploads <- handleFileUploads
  let getTextParam :: Compile.RequestParam -> Snap.Snap Text = \param -> do
        Snap.rqPostParam (cs . show $ param)
          <$> Snap.getRequest
          <&> cs . BS.concat . fromMaybe mempty
  content <- getTextParam Compile.ParamContent
  randomizeVariables <- getTextParam Compile.ParamRandomizeVariables
  outputOption <- getTextParam Compile.ParamOutputOption
  if
    | T.null content
      -> writeJSON $ Error.mk "Problem content cannot be empty"
    | otherwise -> do
        response :: LBS.ByteString <- requestIcemakerCompileProblem
          content
          (Just randomizeVariables)
          (Just outputOption)
          fileUploads
        case JSON.decode response :: Maybe Compile.IcemakerResponse of
          Nothing -> writeJSON $ Error.mk "Something went wrong"
          Just r -> writeJSON Compile.Response
            { Compile.resErrorIcemaker = Compile.errorIcemaker r
            , Compile.resErrorLatex = Compile.errorLatex r
            , Compile.resPdfContent = Compile.pdfContent r
            , Compile.resTerminalOutput = Compile.terminalOutput r
            }
  -- Delete the uploaded files from the server
  IO.liftIO $ forM_ (map filePath fileUploads) $ \fp -> Dir.removeFile fp

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
      response :: LBS.ByteString <- requestIcemakerCompileProblem
        (Problem.content problem)
        randomizeVariables
        outputOption
        [] -- TODO: use files from S3 bucket
      case JSON.decode response :: Maybe Compile.IcemakerResponse of
        Nothing -> writeJSON $ Error.mk "Something went wrong"
        Just r -> writeJSON Compile.Response
          { Compile.resErrorIcemaker = Compile.errorIcemaker r
          , Compile.resErrorLatex = Compile.errorLatex r
          , Compile.resPdfContent = Compile.pdfContent r
          , Compile.resTerminalOutput = Compile.terminalOutput r
          }
  -- Delete the uploaded files from the server
  -- IO.liftIO $ forM_ (map filePath fileUploads) $ \fp -> Dir.removeFile fp

-- | Get the files from the POST request and make them persist in the temporary directory.
-- Returns a list of the files as form parts and their paths.
-- This must be called before getting other POST parameters from the request.
handleFileUploads :: Snap.Snap [FileUpload]
handleFileUploads = do
  tmpDir <- IO.liftIO Dir.getTemporaryDirectory
  let targetDir = tmpDir </> "greatproblems"
  let renameFile info = \case
        Left e -> do
          print e
          return Nothing
        Right fp -> case Snap.partFileName info of
          Nothing -> return Nothing
          Just fileName -> do
            let fp' = targetDir </> cs fileName
            Dir.createDirectoryIfMissing True targetDir
            Dir.renameFile fp fp'
            return . Just $ FileUpload (Wreq.partFile "multiplefiles" fp') fp'
  Snap.handleFileUploads
    tmpDir
    Snap.defaultUploadPolicy
    (const $ Snap.allowWithMaximumSize 20000) -- 20 kB max file size
    renameFile
    <&> catMaybes

data FileUpload = FileUpload
  { filePart :: Wreq.Part
  , filePath :: FilePath
  }
