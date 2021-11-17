module Database.Queries
  ( getProblems
  , getProblemById
  , createProblem
  , updateProblem
  , getTopics
  , getTopicById
  , getRootTopics
  , getTopicsByParentId
  , getUsers
  , getUserById
  , getUserByEmail
  , registerUser
  , getTopicHierarchy
  , authenticate
  , verifyEmail
  , newEmailVerification
  , newResetPassword
  , useResetPassword
  , newSession
  , removeSessionsByUserId
  , removeSessionById
  , getUserFromSession
  , getFigureById
  , getRoles
  , updateUserRole
  , updateUserPassword
  ) where

import Common.Lib.Prelude
import qualified Backend.Lib.Util as Util

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.ToField as SQL
import qualified Data.Text as Text
import qualified Data.CaseInsensitive as CI

import qualified Common.Route as Route
import qualified Common.Api.Problem as Problem
import qualified Common.Api.Topic as Topic
import qualified Common.Api.Register as Register
import qualified Common.Api.User as User
import qualified Common.Api.Role as Role
import qualified Common.Api.Auth as Auth
import qualified Common.Api.Figure as Figure
import qualified Common.Api.ChangePassword as ChangePassword
import qualified Database.Types.Problem as DbProblem
import qualified Database.Types.Topic as DbTopic
import qualified Database.Types.User as DbUser
import qualified Database.Types.Role as DbRole
import qualified Database.Types.EmailVerification as DbEmailVerification
import qualified Database.Types.ResetPassword as DbResetPassword
import qualified Database.Types.Session as DbSession
import qualified Database.Types.Figure as DbFigure

exprFromRouteParam
  :: (SQL.ToField q, Read a)
  => Text -- ^ Param name in route query
  -> SQL.Query -- ^ Target SQL expression with parameter substitution
  -> (a -> q) -- ^ Function to take the previous type to a ToField type (or simply use id with type signature)
  -> Route.Query -- ^ Route query
  -> Maybe (SQL.Query, SQL.Action)
exprFromRouteParam param expr cast routeQuery = do
  y <- Route.readParamFromQuery param routeQuery
  Just (expr, SQL.toField $ cast y)

getProblems :: SQL.Connection -> Maybe Integer -> Route.Query -> IO [Problem.Problem]
getProblems conn problemId routeQuery = do
  let problemExpr = problemId >>= \pid -> Just ("id = ?", SQL.toField pid)
  topicExpr <- case Route.readParamFromQuery "topic" routeQuery of
    Nothing -> return Nothing
    Just topicId -> do
      topicIds <- getTopicIdDescendants conn topicId
      return $ Just ("topic_id IN ?", SQL.toField $ SQL.In topicIds)
  let
    authorExpr = exprFromRouteParam
      "author"
      "author_id = ?"
      (id :: Integer -> Integer)
      routeQuery
    exprs = [problemExpr, topicExpr, authorExpr]
    whereClause = mconcat . intersperse " AND " . map fst . catMaybes $ exprs
    whereParams = map snd . catMaybes $ exprs
  dbProblems <- if not . all isNothing $ exprs
    then SQL.query conn ("SELECT * FROM problems WHERE " <> whereClause) whereParams
    else SQL.query_ conn "SELECT * FROM problems"
  let problems = dbProblems <&> \dbProblem -> Problem.Problem
        { Problem.id = DbProblem.id dbProblem
        , Problem.summary = DbProblem.summary dbProblem
        , Problem.contents = DbProblem.contents dbProblem
        , Problem.topic = Left $ DbProblem.topic_id dbProblem
        , Problem.author = Left $ DbProblem.author_id dbProblem
        , Problem.topicPath = Nothing
        , Problem.figures = []
        , Problem.createdAt = DbProblem.created_at dbProblem
        , Problem.updatedAt = DbProblem.updated_at dbProblem
        }
  let expands = maybe [] (Text.splitOn ",") $ Route.textParamFromQuery "expand" routeQuery
  let includeTopicPath = Route.textParamFromQuery "include" routeQuery == Just "topic_path"
  foldr (>=>) return
    [ sequence . map withFigures
    , Util.whenM ("author" `elem` expands) (sequence . map expandProblemAuthor)
    , Util.whenM ("topic" `elem` expands) (sequence . map expandProblemTopic)
    , Util.whenM includeTopicPath (sequence . map withTopicPath)
    ]
    problems
  where
    expandProblemAuthor :: Problem.Problem -> IO Problem.Problem
    expandProblemAuthor problem = case Problem.author problem of
      Left authorId -> getUserById conn authorId >>= \case
        Nothing -> do
          error $ "expandProblemAuthor: User not found with ID: " ++ show authorId
        Just author -> return $ problem { Problem.author = Right author }
      Right _ -> return problem
    expandProblemTopic :: Problem.Problem -> IO Problem.Problem
    expandProblemTopic problem = case Problem.topic problem of
      Left topicId -> getTopicById conn topicId >>= \case
        Nothing -> do
          error $ "expandProblemAuthor: Topic not found with ID: " ++ show topicId
        Just topic -> return $ problem { Problem.topic = Right topic }
      Right _ -> return problem
    withTopicPath :: Problem.Problem -> IO Problem.Problem
    withTopicPath problem = do
      topic <- case Problem.topic problem of
        Left topicId -> getTopicById conn topicId >>= \case
          Nothing -> do
            error $ "withTopicPath: Topic not found with ID: " ++ show topicId
          Just x -> return x
        Right x -> return x
      topicPath <- getTopicPath conn topic
      return $ problem { Problem.topicPath = Just topicPath}
    withFigures :: Problem.Problem -> IO Problem.Problem
    withFigures problem = do
      figures <- getFiguresByProblemId conn (Problem.id problem)
      return $ problem { Problem.figures = figures }

getProblemById :: SQL.Connection -> Integer -> Route.Query -> IO (Maybe Problem.Problem)
getProblemById conn problemId routeQuery = headMay
  <$> getProblems conn (Just problemId) routeQuery

createProblem :: SQL.Connection -> Problem.BareProblem -> IO (Maybe Problem.Problem)
createProblem conn newProblem = do
  mProblemId :: Maybe (SQL.Only Integer) <- headMay
    <$> SQL.query conn
    "INSERT INTO problems(summary, contents, topic_id, author_id) VALUES (?,?,?,?) returning id"
    ( Problem.bpSummary newProblem
    , Problem.bpContents newProblem
    , Problem.bpTopicId newProblem
    , Problem.bpAuthorId newProblem
    )
  case SQL.fromOnly <$> mProblemId of
    Nothing -> return Nothing
    Just problemId -> do
      forM_ (Problem.bpFigures newProblem) $ \figure -> do
        void $ SQL.execute conn
          "INSERT INTO figures(name, contents, problem_id) VALUES (?,?,?)"
          (Figure.bfName figure, Figure.bfContents figure, problemId)
      getProblemById conn problemId mempty

updateProblem :: SQL.Connection -> Problem.BareProblem -> IO (Maybe Problem.Problem)
updateProblem conn problem = do
  mProblemId :: Maybe (SQL.Only Integer) <- headMay
    <$> SQL.query conn
    "UPDATE problems SET (summary, contents, topic_id, updated_at) = (?, ?, ?, DEFAULT) WHERE id = ? returning id"
    ( Problem.bpSummary problem
    , Problem.bpContents problem
    , Problem.bpTopicId problem
    , Problem.bpProblemId problem
    )
  case SQL.fromOnly <$> mProblemId of
    Nothing -> return Nothing
    Just problemId -> do
      void $ SQL.execute conn
        "DELETE FROM figures WHERE problem_id = ?"
        (SQL.Only problemId)
      forM_ (Problem.bpFigures problem) $ \figure -> do
        void $ SQL.execute conn
          "INSERT INTO figures(name, contents, problem_id) VALUES (?,?,?)"
          (Figure.bfName figure, Figure.bfContents figure, problemId)
      getProblemById conn problemId mempty

getTopics :: SQL.Connection -> IO [Topic.Topic]
getTopics conn = do
  dbTopics <- SQL.query_ conn "SELECT * FROM topics"
  return $ dbTopics <&> \dbTopic -> Topic.Topic
    (DbTopic.id dbTopic)
    (DbTopic.name dbTopic)
    (DbTopic.parent_id dbTopic)

getTopicById :: SQL.Connection -> Integer -> IO (Maybe Topic.Topic)
getTopicById conn topicId = do
  mDbTopic <- headMay
    <$> SQL.query conn "SELECT * FROM topics WHERE id = ?" (SQL.Only topicId)
  case mDbTopic of
    Nothing -> return Nothing
    Just dbTopic -> return . Just $ Topic.Topic
      (DbTopic.id dbTopic)
      (DbTopic.name dbTopic)
      (DbTopic.parent_id dbTopic)

getParentTopic :: SQL.Connection -> Topic.Topic -> IO (Maybe Topic.Topic)
getParentTopic conn topic = case Topic.parentId topic of
  Nothing -> return Nothing
  Just parentId -> do
    mDbTopic <- headMay <$> SQL.query conn
      "SELECT * FROM topics WHERE id = ?"
      (SQL.Only parentId)
    case mDbTopic of
      Nothing -> return Nothing
      Just dbTopic -> return . Just $ Topic.Topic
        (DbTopic.id dbTopic)
        (DbTopic.name dbTopic)
        (DbTopic.parent_id dbTopic)

getRootTopics :: SQL.Connection -> IO [Topic.Topic]
getRootTopics conn = do
  dbTopics <- SQL.query_ conn "SELECT * FROM topics WHERE parent_id IS NULL"
  return $ dbTopics <&> \dbTopic -> Topic.Topic
    (DbTopic.id dbTopic)
    (DbTopic.name dbTopic)
    (DbTopic.parent_id dbTopic)

getTopicsByParentId :: SQL.Connection -> Integer -> IO [Topic.Topic]
getTopicsByParentId conn parentId = do
  dbTopics <- SQL.query conn "SELECT * FROM topics WHERE parent_id = ?" (SQL.Only parentId)
  return $ dbTopics <&> \dbTopic -> Topic.Topic
    (DbTopic.id dbTopic)
    (DbTopic.name dbTopic)
    (DbTopic.parent_id dbTopic)

-- | Get the path pertaining to a given topic, tracing its path to a root topic.
-- For example, given the topic Limits, return [Mathematics, Calculus, Limits].
getTopicPath :: SQL.Connection -> Topic.Topic -> IO [Topic.Topic]
getTopicPath conn topic
  | isNothing (Topic.parentId topic) = return [topic]
  | otherwise = do
      topicPath <- getParentTopic conn topic >>= \case
        Nothing -> return []
        Just parent -> getTopicPath conn parent
      return $ topicPath ++ [topic]

-- | Get all the descendants of a given topic as a flat list.
getTopicDescendants :: SQL.Connection -> Topic.Topic -> IO [Topic.Topic]
getTopicDescendants conn topic = do
  (SQL.query conn "SELECT * FROM topics WHERE parent_id = ?" (SQL.Only $ Topic.id topic)) >>= \case
    [] -> return [topic]
    dbChildren -> do
      let children = dbChildren <&> \dbTopic -> Topic.Topic
            (DbTopic.id dbTopic)
            (DbTopic.name dbTopic)
            (DbTopic.parent_id dbTopic)
      ds <- mapM (getTopicDescendants conn) children
      return $ topic : concat ds

-- | Same as getTopicDescendants, but only IDs.
getTopicIdDescendants :: SQL.Connection -> Integer -> IO [Integer]
getTopicIdDescendants conn topicId = do
  getTopicById conn topicId >>= \case
    Nothing -> return []
    Just topic -> do
      topics <- getTopicDescendants conn topic
      return $ map Topic.id topics

-- | Get the hierarchy of topics, ending with the children of the given topic. The Either type is used to keep track of unselected and selected topics, respectively Left and Right.
-- For example, given the topic Mathematics, return
--   [ [Left Astronomy, Left Biology, Left Chemistry, Left (Electrical Engineering), Right Mathematics, Left Physics, Left Psychology, Left Statistics]
--   , [Right Calculus, Left (Group Theory)]
--   , [Left (Differential Equations), Left Limits, Left Rates]
--   ]
-- For example, given the topic Calculus, return
--   [ [Left Astronomy, Left Biology, Left Chemistry, Left (Electrical Engineering), Right Mathematics, Left Physics, Left Psychology, Left Statistics]
--   , [Right Calculus, Left (Group Theory)]
--   , [Left (Differential Equations), Left Limits, Left Rates]
--   ]
-- For example, given the topic Limits, return
--   [ [Left Astronomy, Left Biology, Left Chemistry, Left (Electrical Engineering), Right Mathematics, Left Physics, Left Psychology, Left Statistics]
--   , [Right Calculus, Left (Group Theory)]
--   , [Left (Differential Equations), Right Limits, Left Rates]
--   , []
--   ]
getTopicHierarchy :: SQL.Connection -> Topic.Topic -> IO [[Either Topic.Topic Topic.Topic]]
getTopicHierarchy conn topic = do
  topicPath <- getTopicPath conn topic
  siblings <- mapM getSiblings topicPath
  children <- getChildren topic
  return $ siblings ++ [children]
  where
    getSiblings :: Topic.Topic -> IO [Either Topic.Topic Topic.Topic]
    getSiblings t = do
      xs <- case Topic.parentId t of
        Nothing -> getRootTopics conn
        Just pid -> getTopicsByParentId conn pid
      return $ map (\x -> if x == t then Right x else Left x) xs
      
    getChildren :: Topic.Topic -> IO [Either Topic.Topic Topic.Topic]
    getChildren t = do
      xs <- getTopicsByParentId conn (Topic.id t)
      return $ map Left xs

getRoles :: SQL.Connection -> IO [Role.Role]
getRoles conn = do
  dbRoles <- SQL.query_ conn "SELECT * FROM roles"
  return $ dbRoles <&> \case
    DbRole.Role _ "Basic" -> Role.Basic
    DbRole.Role _ "Contributor" -> Role.Contributor
    DbRole.Role _ "Moderator" -> Role.Moderator
    DbRole.Role _ "Administrator" -> Role.Administrator
    _ -> Role.Basic

getRoleById :: SQL.Connection -> Integer -> IO (Maybe Role.Role)
getRoleById conn roleId = do
  mDbRole :: Maybe DbRole.Role <- headMay
    <$> SQL.query conn "SELECT * FROM roles WHERE id = ?" (SQL.Only roleId)
  return $ flip fmap mDbRole $ \case
    DbRole.Role _ "Basic" -> Role.Basic
    DbRole.Role _ "Contributor" -> Role.Contributor
    DbRole.Role _ "Moderator" -> Role.Moderator
    DbRole.Role _ "Administrator" -> Role.Administrator
    _ -> Role.Basic

getUsers :: SQL.Connection -> IO [User.User]
getUsers conn = do
  dbUsers :: [DbUser.User] <- SQL.query_ conn "SELECT * FROM users ORDER BY id ASC"
  flip mapM dbUsers $ \dbUser -> do
    getRoleById conn (DbUser.role_id dbUser) >>= \case
      Nothing -> return $ User.User
        { User.id = DbUser.id dbUser
        , User.fullName = DbUser.full_name dbUser
        , User.email = DbUser.email dbUser
        , User.role = Role.Basic
        , User.verified = DbUser.verified dbUser
        }
      Just role -> return $ User.User
        { User.id = DbUser.id dbUser
        , User.fullName = DbUser.full_name dbUser
        , User.email = DbUser.email dbUser
        , User.role = role
        , User.verified = DbUser.verified dbUser
        }

getUserById :: SQL.Connection -> Integer -> IO (Maybe User.User)
getUserById conn userId = do
  headMay
    <$> SQL.query conn "SELECT * FROM users WHERE id = ?" (SQL.Only userId)
    >>= \case
    Nothing -> return Nothing
    Just (dbUser :: DbUser.User) -> withRole dbUser <$> getRoleById conn (DbUser.role_id dbUser)

getUserByEmail :: SQL.Connection -> CI Text -> IO (Maybe User.User)
getUserByEmail conn email = do
  headMay
    <$> SQL.query conn "SELECT * FROM users WHERE email = ?" (SQL.Only $ CI.original email)
    >>= \case
    Nothing -> return Nothing
    Just (dbUser :: DbUser.User) -> withRole dbUser <$> getRoleById conn (DbUser.role_id dbUser)

registerUser :: SQL.Connection -> Register.Register -> IO (Maybe User.User)
registerUser conn user = do
  password <- Util.hashPassword $ Register.password user
  role :: DbRole.Role <- head <$> SQL.query conn "SELECT * FROM roles WHERE name = ?" (SQL.Only $ show Role.Basic)
  mUserId :: Maybe (SQL.Only Integer) <- headMay
    <$> SQL.query conn
    "INSERT INTO users(full_name, email, password, role_id) VALUES (?,?,?,?) returning id"
    ( Register.fullName user
    , Register.email user
    , password
    , DbRole.id role
    )
  flip (maybe (pure Nothing))
    (SQL.fromOnly <$> mUserId)
    $ \userId -> getUserById conn userId

withRole :: DbUser.User -> Maybe Role.Role -> Maybe User.User
withRole dbUser mRole = do
  role <- mRole
  return User.User
    { User.id = DbUser.id dbUser
    , User.fullName = DbUser.full_name dbUser
    , User.email = DbUser.email dbUser
    , User.role = role
    , User.verified = DbUser.verified dbUser
    }

authenticate :: SQL.Connection -> Auth.Auth -> IO (Maybe User.User)
authenticate conn auth = do
  headMay
    <$> SQL.query conn "SELECT * FROM users WHERE email = ?" (SQL.Only (Auth.email auth))
    >>= \case
    Nothing -> return Nothing
    Just (dbUser :: DbUser.User) ->
      if Util.verifyPassword (Auth.password auth) (DbUser.password dbUser)
        then withRole dbUser <$> getRoleById conn (DbUser.role_id dbUser)
        else return Nothing

getEmailVerificationBySecret
  :: SQL.Connection
  -> Text
  -> IO (Maybe DbEmailVerification.EmailVerification)
getEmailVerificationBySecret conn secret = headMay
  <$> SQL.query conn "SELECT * FROM email_verifications WHERE secret = ?" (SQL.Only secret)

verifyEmail :: SQL.Connection -> Text -> IO Bool
verifyEmail conn secret = do
  getEmailVerificationBySecret conn secret
    >>= \case
    Nothing -> return False
    Just (x :: DbEmailVerification.EmailVerification) -> do
      setUserVerified conn (DbEmailVerification.user_id x)
      void $ SQL.execute conn "DELETE FROM email_verifications WHERE id = ?"
        (SQL.Only $ DbEmailVerification.id x)
      return True

setUserVerified :: SQL.Connection -> Integer -> IO ()
setUserVerified conn userId = void
  $ SQL.execute conn "UPDATE users SET verified = TRUE WHERE id = ?" (SQL.Only userId)

newEmailVerification
  :: SQL.Connection
  -> Integer -- ^ User ID
  -> IO Text -- ^ Secret
newEmailVerification conn userId = do
  let generateSecret = do
        secret <- Util.generateRandomText
        getEmailVerificationBySecret conn secret >>= \case
          Nothing -> return secret
          Just _ -> generateSecret
  secret <- generateSecret
  void $ SQL.execute conn
    "INSERT INTO email_verifications(secret, user_id) VALUES (?,?)" (secret, userId)
  return secret

newResetPassword
  :: SQL.Connection
  -> Integer -- ^ User ID
  -> IO Text -- ^ Secret
newResetPassword conn userId = do
  let generateSecret = do
        secret <- Util.generateRandomText
        getEmailVerificationBySecret conn secret >>= \case
          Nothing -> return secret
          Just _ -> generateSecret
  secret <- generateSecret
  void $ SQL.execute conn
    "INSERT INTO reset_password(secret, user_id) VALUES (?,?)" (secret, userId)
  return secret

getResetPasswordBySecret
  :: SQL.Connection
  -> Text
  -> IO (Maybe DbResetPassword.ResetPassword)
getResetPasswordBySecret conn secret = headMay
  <$> SQL.query conn "SELECT * FROM reset_password WHERE secret = ?" (SQL.Only secret)

checkResetPasswordExpired
  :: SQL.Connection
  -> Integer -- ^ ID
  -> IO Bool
checkResetPasswordExpired conn resetPasswordId = headMay
  <$> SQL.query conn
  "SELECT * FROM reset_password WHERE id = ? AND (created_at > NOW() - interval '30 minutes')"
  (SQL.Only resetPasswordId)
  >>= \case
  Nothing -> return True
  Just (_ :: DbResetPassword.ResetPassword) -> return False

useResetPassword
  :: SQL.Connection
  -> Text
  -> IO (Maybe User.User)
useResetPassword conn secret = do
  getResetPasswordBySecret conn secret >>= \case
    Nothing -> return Nothing
    Just rp -> checkResetPasswordExpired conn (DbResetPassword.id rp) >>= \case
      True -> do
        void $ SQL.execute conn "DELETE FROM reset_password WHERE id = ?"
          (SQL.Only $ DbResetPassword.id rp)
        return Nothing
      False -> do
        void $ SQL.execute conn "DELETE FROM reset_password WHERE id = ?"
          (SQL.Only $ DbResetPassword.id rp)
        getUserById conn (DbResetPassword.user_id rp) >>= \case
          Nothing -> return Nothing
          Just user -> return $ Just user

getSessionById :: SQL.Connection -> Text -> IO (Maybe DbSession.Session)
getSessionById conn sessionId = headMay
  <$> SQL.query conn "SELECT * FROM sessions WHERE id = ?" (SQL.Only sessionId)

removeSessionsByUserId :: SQL.Connection -> Integer -> IO ()
removeSessionsByUserId conn userId = void $ SQL.execute conn
  "DELETE FROM sessions WHERE user_id = ?" (SQL.Only userId)

newSession :: SQL.Connection -> Integer -> IO Text
newSession conn userId = do
  let generateSessionId = do
        sessionId <- Util.generateRandomText
        getSessionById conn sessionId >>= \case
          Nothing -> return sessionId
          Just _ -> generateSessionId
  sessionId <- generateSessionId
  void $ SQL.execute conn
    "INSERT INTO sessions(id, user_id) VALUES (?,?)" (sessionId, userId)
  return sessionId

removeSessionById :: SQL.Connection -> Text -> IO ()
removeSessionById conn sessionId = void $ SQL.execute conn
  "DELETE FROM sessions WHERE id = ?" (SQL.Only sessionId)

getUserFromSession :: SQL.Connection -> Text -> IO (Maybe User.User)
getUserFromSession conn sessionId = do
  getSessionById conn sessionId >>= \case
    Nothing -> return Nothing
    Just session -> getUserById conn (DbSession.user_id session)

getFiguresByProblemId :: SQL.Connection -> Integer -> IO [Figure.Figure]
getFiguresByProblemId conn problemId = do
  dbFigures <- SQL.query conn
    "SELECT * FROM figures WHERE problem_id = ?"
    (SQL.Only problemId)
  return $ dbFigures <&> \dbFigure -> Figure.Figure
    { Figure.id = DbFigure.id dbFigure
    , Figure.name = DbFigure.name dbFigure
    , Figure.contents = DbFigure.contents dbFigure
    , Figure.createdAt = DbFigure.created_at dbFigure
    , Figure.updatedAt = DbFigure.updated_at dbFigure
    }

getFigureById :: SQL.Connection -> Integer -> IO (Maybe Figure.Figure)
getFigureById conn figureId = do
  mDbFigure <- headMay
    <$> SQL.query conn "SELECT * FROM figures WHERE id = ?" (SQL.Only figureId)
  case mDbFigure of
    Nothing -> return Nothing
    Just dbFigure -> return . Just $ Figure.Figure
      { Figure.id = DbFigure.id dbFigure
      , Figure.name = DbFigure.name dbFigure
      , Figure.contents = DbFigure.contents dbFigure
      , Figure.createdAt = DbFigure.created_at dbFigure
      , Figure.updatedAt = DbFigure.updated_at dbFigure
      }

updateUserRole :: SQL.Connection -> Integer -> Role.Role -> IO (Maybe User.User)
updateUserRole conn userId newRole = do
  mDbRole :: Maybe DbRole.Role <- headMay
    <$> SQL.query conn "SELECT * FROM roles WHERE name = ?" (SQL.Only $ show newRole)
  case mDbRole of
    Nothing -> return Nothing
    Just dbRole -> do
      void $ SQL.execute conn
        "UPDATE users SET role_id = ? WHERE id = ?"
        (DbRole.id dbRole, userId)
      getUserById conn userId

updateUserPassword :: SQL.Connection -> Integer -> ChangePassword.ChangePassword -> IO (Maybe User.User)
updateUserPassword conn userId changePassword = do
  newPassword <- Util.hashPassword (ChangePassword.newPassword changePassword)
  void $ SQL.execute conn
    "UPDATE users SET password = ? WHERE id = ?"
    (newPassword, userId)
  getUserById conn userId
