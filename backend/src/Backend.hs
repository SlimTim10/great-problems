{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Lib.Prelude

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
import qualified Common.Api.ProblemStatus as ProblemStatus
import qualified Common.Api.Figure as Figure
import qualified Common.Api.ChangePassword as ChangePassword
import qualified Auth
import qualified Email

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
                Snap.GET -> writeJSON =<< IO.liftIO (Queries.getProblems conn query)
                Snap.POST -> case mUser of
                  Nothing -> writeJSON $ Error.mk "No access"
                  Just user -> handleSaveProblem conn user
                _ -> return () -- TODO: implement put, delete
                
            Route.Api_Problems :/ (Just problemId, _) -> do
              writeJSON =<< IO.liftIO (Queries.getProblemById conn problemId)
              
            Route.Api_Topics :/ query -> do
              topics <- case fromMaybe Nothing (Map.lookup "parent" query) of
                Just "null" -> IO.liftIO $ Queries.getRootTopics conn
                Just x -> case readMaybe (cs x) :: Maybe Integer of
                  Just parentId -> IO.liftIO $ Queries.getTopicsByParentId conn parentId
                  Nothing -> IO.liftIO $ Queries.getTopics conn
                Nothing -> IO.liftIO $ Queries.getTopics conn
              writeJSON topics
              
            Route.Api_Users :/ Nothing -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> case mUser of
                  Nothing -> writeJSON $ Error.mk "No access"
                  Just user -> case User.role user of
                    Role.Administrator -> writeJSON =<< IO.liftIO (Queries.getUsers conn)
                    _ -> writeJSON $ Error.mk "No access"
                _ -> return ()
                
            Route.Api_Users :/ (Just userId) -> do
              Snap.rqMethod <$> Snap.getRequest >>= \case
                Snap.GET -> writeJSON =<< IO.liftIO (Queries.getUserById conn userId)
                Snap.POST -> case mUser of
                  Nothing -> writeJSON $ Error.mk "No access"
                  Just user -> case User.role user of
                    Role.Administrator -> updateUser conn userId
                    _ -> writeJSON $ Error.mk "No access"
                _ -> return ()
                
            Route.Api_Roles :/ () -> case mUser of
              Nothing -> writeJSON $ Error.mk "No access"
              Just user -> case User.role user of
                Role.Administrator -> writeJSON =<< IO.liftIO (Queries.getRoles conn)
                _ -> writeJSON $ Error.mk "No access"
                
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
                else writeJSON $ Error.mk "Invalid email verification link"
                
            Route.Api_ChangePassword :/ () -> Snap.rqMethod <$> Snap.getRequest >>= \case
              Snap.POST -> do
                rawBody <- Snap.readRequestBody maxRequestBodySize
                case JSON.decode rawBody :: Maybe ChangePassword.ChangePassword of
                  Nothing -> writeJSON $ Error.mk "Something went wrong"
                  Just cp -> do
                    let changePassword = \user newPassword -> do
                          if T.null newPassword
                            then writeJSON $ Error.mk "Password cannot be empty"
                            else do
                            IO.liftIO (Queries.updateUserPassword conn (User.id user) cp) >>= \case
                              Nothing -> writeJSON $ Error.mk "Something went wrong"
                              Just _ -> writeJSON OkResponse.OkResponse
                    case ChangePassword.identification cp of
                      ChangePassword.OldPassword oldPassword -> do
                        case mUser of
                          Nothing -> writeJSON $ Error.mk "No access"
                          Just user -> do
                            let auth = Auth.Auth (User.email user) oldPassword
                            IO.liftIO (Auth.authCheck conn auth) >>= \case
                              Auth.Authenticated _ -> changePassword  user (ChangePassword.newPassword cp)
                              _ -> writeJSON $ Error.mk "Incorrect password"
                      ChangePassword.ResetSecret secret -> do
                        IO.liftIO (Queries.useResetPassword conn secret) >>= \case
                          Nothing -> writeJSON $ Error.mk "Expired or invalid link"
                          Just user -> changePassword user (ChangePassword.newPassword cp)
              _ -> return ()
                
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

            Route.Api_ResetPassword :/ () -> Snap.rqMethod <$> Snap.getRequest >>= \case
              Snap.POST -> do
                rawBody <- Snap.readRequestBody maxRequestBodySize
                case JSON.decode rawBody :: Maybe User.ResetPasswordRequest of
                  Nothing -> writeJSON $ Error.mk "Something went wrong"
                  Just req -> do
                    let email = User.rprEmail req
                    IO.liftIO (Queries.getUserByEmail conn email) >>= \case
                      -- Don't reveal that the email was not found (for security)
                      Nothing -> writeJSON OkResponse.OkResponse
                      Just user -> do
                        secret <- IO.liftIO $ Queries.newResetPassword conn (User.id user)
                        Email.sendResetPasswordEmail user secret
                        writeJSON OkResponse.OkResponse
              _ -> return ()
              
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
  where
    updateUser conn userId = do
      rawBody <- Snap.readRequestBody maxRequestBodySize
      case JSON.decode rawBody :: Maybe (User.UpdateRequest) of
        Nothing -> writeJSON $ Error.mk "Something went wrong"
        Just req -> do
          IO.liftIO (Queries.updateUserRole conn userId (User.urRole req)) >>= \case
            Nothing -> writeJSON $ Error.mk "Something went wrong"
            Just _ -> writeJSON OkResponse.OkResponse

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
  { Snap.cookieName = cs name :: BS.ByteString
  , Snap.cookieValue = cs value :: BS.ByteString
  , Snap.cookieExpires = Nothing
  , Snap.cookieDomain = Nothing
  , Snap.cookiePath = Just "/"
  , Snap.cookieSecure = True
  , Snap.cookieHttpOnly = False
  }

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
  status <- getTextParam Problem.ParamStatus
  existingAuthor :: Maybe User.User <- IO.liftIO $ do
    if T.null problemId
      then return Nothing
      else do
      (Queries.getProblemById conn (read . cs $ problemId)) >>= \case
        Nothing -> return Nothing
        Just p -> return . Just $ Problem.author p
  if
    | any not
      [ T.null problemId || (read . cs $ authorId) == User.id user
      , T.null problemId || (User.id <$> existingAuthor) == Just (User.id user)
      , User.role user `elem` [Role.Contributor, Role.Moderator, Role.Administrator]
      ]
      -> writeJSON $ Error.mk "No access"
    | T.null contents
      -> writeJSON $ Error.mk "Problem contents cannot be empty"
    | T.null summary
      -> writeJSON $ Error.mk "Summary cannot be empty"
    | (read . cs $ status) == ProblemStatus.Published
      -> do
        -- Published problems must compile without errors
        response :: LBS.ByteString <- requestProblem2texCompileProblem
          contents
          Nothing
          Nothing
          figures
        case JSON.eitherDecode response :: Either String Compile.Problem2texResponse of
          Left e -> do
            IO.liftIO $ putStrLn "Error response:"
            IO.liftIO $ print e
            writeJSON $ Error.mk "Something went wrong"
          Right r -> if
            | (not . T.null $ Compile.p2tErrorProblem2tex r) || (not . T.null $ Compile.p2tErrorLatex r)
              -> writeJSON $ Error.mk "Invalid problem. Please check that your problem compiles with no errors before saving."
            | T.null problemId -> do
                createNewProblem
                  Problem.BareProblem
                  { Problem.bpProblemId = Nothing
                  , Problem.bpSummary = summary
                  , Problem.bpContents = contents
                  , Problem.bpTopicId = read . cs $ topicId
                  , Problem.bpAuthorId = read . cs $ authorId
                  , Problem.bpStatus = read . cs $ status
                  , Problem.bpFigures = figures
                  }
            | otherwise -> do
                updateExistingProblem
                  Problem.BareProblem
                  { Problem.bpProblemId = Just $ (read . cs $ problemId :: Integer)
                  , Problem.bpSummary = summary
                  , Problem.bpContents = contents
                  , Problem.bpTopicId = read . cs $ topicId
                  , Problem.bpAuthorId = read . cs $ authorId
                  , Problem.bpStatus = read . cs $ status
                  , Problem.bpFigures = figures
                  }
    | otherwise -> do
        if
          | T.null problemId -> do
              createNewProblem
                Problem.BareProblem
                { Problem.bpProblemId = Nothing
                , Problem.bpSummary = summary
                , Problem.bpContents = contents
                , Problem.bpTopicId = read . cs $ topicId
                , Problem.bpAuthorId = read . cs $ authorId
                , Problem.bpStatus = read . cs $ status
                , Problem.bpFigures = figures
                }
          | otherwise -> do
              updateExistingProblem
                Problem.BareProblem
                { Problem.bpProblemId = Just $ (read . cs $ problemId :: Integer)
                , Problem.bpSummary = summary
                , Problem.bpContents = contents
                , Problem.bpTopicId = read . cs $ topicId
                , Problem.bpAuthorId = read . cs $ authorId
                , Problem.bpStatus = read . cs $ status
                , Problem.bpFigures = figures
                }
  where
    createNewProblem problem = do
      IO.liftIO (Queries.createProblem conn problem) >>= \case
        Nothing -> writeJSON $ Error.mk "Something went wrong"
        Just createdProblem -> writeJSON createdProblem
        
    updateExistingProblem problem = do
      IO.liftIO (Queries.updateProblem conn problem) >>= \case
        Nothing -> writeJSON $ Error.mk "Something went wrong"
        Just updatedProblem -> writeJSON updatedProblem


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
        response :: LBS.ByteString <- requestProblem2texCompileProblem
          contents
          (Just randomizeVariables)
          (Just outputOption)
          figures
        case JSON.eitherDecode response :: Either String Compile.Problem2texResponse of
          Left e -> do
            IO.liftIO $ putStrLn "Error response:"
            IO.liftIO $ print e
            writeJSON $ Error.mk "Something went wrong"
          Right r -> writeJSON Compile.Response
            { Compile.resErrorProblem2tex = Compile.p2tErrorProblem2tex r
            , Compile.resErrorLatex = Compile.p2tErrorLatex r
            , Compile.resPdfContents = Compile.p2tPdfContents r
            , Compile.resTerminalOutput = Compile.p2tTerminalOutput r
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
  IO.liftIO (Queries.getProblemById conn problemId) >>= \case
    Nothing -> writeJSON $ Error.mk "Problem does not exist"
    Just problem -> do
      let figures = Problem.figures problem <&> \figure ->
            Figure.BareFigure
            { Figure.bfName = Figure.name figure
            , Figure.bfContents = Figure.contents figure
            }
      response :: LBS.ByteString <- requestProblem2texCompileProblem
        (Problem.contents problem)
        randomizeVariables
        outputOption
        figures
      case JSON.eitherDecode response :: Either String Compile.Problem2texResponse of
        Left e -> do
          IO.liftIO $ putStrLn "Error response:"
          IO.liftIO $ print e
          writeJSON $ Error.mk "Something went wrong"
        Right r -> writeJSON Compile.Response
          { Compile.resErrorProblem2tex = Compile.p2tErrorProblem2tex r
          , Compile.resErrorLatex = Compile.p2tErrorLatex r
          , Compile.resPdfContents = Compile.p2tPdfContents r
          , Compile.resTerminalOutput = Compile.p2tTerminalOutput r
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
