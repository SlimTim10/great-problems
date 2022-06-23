module Database.Queries
  
  -- problems, figures
  ( createProblem
  , getProblems
  , getProblemById
  , updateProblem
  , deleteProblem
  , getFigureById
  
  -- problem sets
  , createProblemSet
  , getProblemSets
  , getProblemSetById
  , updateProblemSet
  , deleteProblemSet
  
  -- topics
  , createTopic
  , getTopics
  , getTopicById
  , getRootTopics
  , getTopicsByParentId
  , getTopicHierarchy
  , updateTopic
  , deleteTopic
  
  -- users, roles, sessions
  , getUsers
  , getUserById
  , getUserByEmail
  , registerUser
  , authenticate
  , verifyEmail
  , newEmailVerification
  , newResetPassword
  , useResetPassword
  , newSession
  , removeSessionsByUserId
  , removeSessionById
  , getUserFromSession
  , getRoles
  , updateUserRole
  , updateUserPassword

  -- meta settings
  , getMetaSettings
  , getMetaSetting
  , setMetaSetting
  ) where

import Common.Lib.Prelude
import qualified Backend.Lib.Util as Util

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.ToField as SQL
import qualified Data.CaseInsensitive as CI

import qualified Common.Route as Route
import qualified Common.Api.Problem as Problem
import qualified Common.Api.ProblemSet as ProblemSet
import qualified Common.Api.Topic as Topic
import qualified Common.Api.User as User
import qualified Common.Api.Role as Role
import qualified Common.Api.Figure as Figure
import qualified Common.Api.ProblemStatus as ProblemStatus
import qualified Common.Api.MetaSetting as MetaSetting
import qualified Common.Api.Request.Auth as Auth
import qualified Common.Api.Request.ChangePassword as ChangePassword
import qualified Common.Api.Request.Register as Register
import qualified Database.Types.Problem as DbProblem
import qualified Database.Types.ProblemSet as DbProblemSet
import qualified Database.Types.Topic as DbTopic
import qualified Database.Types.User as DbUser
import qualified Database.Types.Role as DbRole
import qualified Database.Types.ProblemStatus as DbProblemStatus
import qualified Database.Types.EmailVerification as DbEmailVerification
import qualified Database.Types.ResetPassword as DbResetPassword
import qualified Database.Types.Session as DbSession
import qualified Database.Types.Figure as DbFigure
import qualified Database.Types.MetaSetting as DbMetaSetting

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

getProblems :: SQL.Connection -> Route.Query -> IO [Problem.Problem]
getProblems conn routeQuery = do
  topicExpr <- case Route.readParamFromQuery "topic" routeQuery of
    Nothing -> return Nothing
    Just topicId -> do
      topicIds <- getTopicIdDescendants conn topicId
      return $ Just ("topic_id IN ?", SQL.toField $ SQL.In topicIds)
  let authorExpr = exprFromRouteParam
        "author"
        "author_id = ?"
        (id :: Integer -> Integer)
        routeQuery
  let statusExpr = exprFromRouteParam
        "status"
        "status_id = ?"
        (id :: Integer -> Integer)
        routeQuery
  queryExpr <- case Route.textParamFromQuery "q" routeQuery of
    Nothing -> return Nothing
    Just q -> return $ Just ("summary ILIKE CONCAT('%', ?, '%')", SQL.toField q)
  let exprs = [topicExpr, authorExpr, statusExpr, queryExpr]
  let whereClause = mconcat . intersperse " AND " . map fst . catMaybes $ exprs
  let orderByClause = " ORDER BY updated_at DESC "
  let whereParams = map snd . catMaybes $ exprs
  dbProblems :: [DbProblem.Problem] <-
    if not . all isNothing $ exprs
    then SQL.query conn ("SELECT * FROM problems WHERE " <> whereClause <> orderByClause) whereParams
    else SQL.query_ conn ("SELECT * FROM problems " <> orderByClause)
  problemAuthors :: [User.User] <- sequence (map (fetchProblemAuthor conn) dbProblems)
  problemTopics :: [Topic.Topic] <- sequence (map (fetchProblemTopic conn) dbProblems)
  problemTopicPaths :: [[Topic.Topic]] <- sequence (map (fetchProblemTopicPath conn) dbProblems)
  problemStatuses :: [ProblemStatus.Status] <- sequence (map (fetchProblemStatus conn) dbProblems)
  problemFiguress :: [[Figure.Figure]] <- sequence (map (fetchProblemFigures conn) dbProblems)
  return
    $ flip map
    (zip6
      dbProblems
      problemAuthors
      problemTopics
      problemTopicPaths
      problemStatuses
      problemFiguress
    )
    $ \(dbProblem, problemAuthor, problemTopic, problemTopicPath, problemStatus, problemFigures) ->
        Problem.Problem
        { Problem.id = DbProblem.id dbProblem
        , Problem.summary = DbProblem.summary dbProblem
        , Problem.contents = DbProblem.contents dbProblem
        , Problem.topic = problemTopic
        , Problem.author = problemAuthor
        , Problem.status = problemStatus
        , Problem.topicPath = problemTopicPath
        , Problem.figures = problemFigures
        , Problem.createdAt = DbProblem.created_at dbProblem
        , Problem.updatedAt = DbProblem.updated_at dbProblem
        }

fetchProblemAuthor :: SQL.Connection -> DbProblem.Problem -> IO User.User
fetchProblemAuthor conn (DbProblem.Problem {DbProblem.author_id=authorId}) =
  getUserById conn authorId
  >>= return . fromMaybe
  (error $ "fetchProblemAuthor: User not found with ID: " ++ show authorId)

fetchProblemTopic :: SQL.Connection -> DbProblem.Problem -> IO Topic.Topic
fetchProblemTopic conn (DbProblem.Problem {DbProblem.topic_id=topicId}) =
  getTopicById conn topicId
  >>= return . fromMaybe
  (error $ "fetchProblemTopic: Topic not found with ID: " ++ show topicId)

fetchProblemTopicPath :: SQL.Connection -> DbProblem.Problem -> IO [Topic.Topic]
fetchProblemTopicPath conn dbProblem = do
  t <- fetchProblemTopic conn dbProblem
  getTopicPath conn t
    
fetchProblemStatus :: SQL.Connection -> DbProblem.Problem -> IO ProblemStatus.Status
fetchProblemStatus conn (DbProblem.Problem {DbProblem.status_id=statusId}) =
  getProblemStatusById conn statusId
  >>= return . fromMaybe
  (error $ "fetchProblemStatus: Status not found with ID: " ++ show statusId)

fetchProblemFigures :: SQL.Connection -> DbProblem.Problem -> IO [Figure.Figure]
fetchProblemFigures conn (DbProblem.Problem {DbProblem.id=pid}) =
  getFiguresByProblemId conn pid

getProblemById :: SQL.Connection -> Integer -> IO (Maybe Problem.Problem)
getProblemById conn problemId = do
  mDbProblem <- headMay
    <$> SQL.query conn "SELECT * FROM problems WHERE id = ?" (SQL.Only problemId)
  case mDbProblem of
    Nothing -> return Nothing
    Just dbProblem -> do
      problemAuthor <- fetchProblemAuthor conn dbProblem
      problemTopic <- fetchProblemTopic conn dbProblem
      problemTopicPath <- fetchProblemTopicPath conn dbProblem
      problemStatus <- fetchProblemStatus conn dbProblem
      problemFigures <- fetchProblemFigures conn dbProblem
      return . Just $
        Problem.Problem
        { Problem.id = DbProblem.id dbProblem
        , Problem.summary = DbProblem.summary dbProblem
        , Problem.contents = DbProblem.contents dbProblem
        , Problem.topic = problemTopic
        , Problem.author = problemAuthor
        , Problem.status = problemStatus
        , Problem.topicPath = problemTopicPath
        , Problem.figures = problemFigures
        , Problem.createdAt = DbProblem.created_at dbProblem
        , Problem.updatedAt = DbProblem.updated_at dbProblem
        }

createProblem :: SQL.Connection -> Problem.BareProblem -> IO (Maybe Problem.Problem)
createProblem conn newProblem = do
  let statusId = ProblemStatus.toId $ Problem.bpStatus newProblem
  mProblemId :: Maybe (SQL.Only Integer) <- headMay
    <$> SQL.query conn
    "INSERT INTO problems(summary, contents, topic_id, author_id, status_id) VALUES (?,?,?,?,?) returning id"
    ( Problem.bpSummary newProblem
    , Problem.bpContents newProblem
    , Problem.bpTopicId newProblem
    , Problem.bpAuthorId newProblem
    , statusId
    )
  case SQL.fromOnly <$> mProblemId of
    Nothing -> return Nothing
    Just problemId -> do
      forM_ (Problem.bpFigures newProblem) $ \figure -> do
        void $ SQL.execute conn
          "INSERT INTO figures(name, contents, problem_id) VALUES (?,?,?)"
          (Figure.bfName figure, SQL.Binary (Figure.bfContents figure), problemId)
      getProblemById conn problemId

updateProblem :: SQL.Connection -> Problem.BareProblem -> IO (Maybe Problem.Problem)
updateProblem conn problem = do
  let statusId = ProblemStatus.toId $ Problem.bpStatus problem
  mProblemId :: Maybe (SQL.Only Integer) <- headMay
    <$> SQL.query conn
    "UPDATE problems SET (summary, contents, topic_id, status_id, updated_at) = (?, ?, ?, ?, DEFAULT) WHERE id = ? returning id"
    ( Problem.bpSummary problem
    , Problem.bpContents problem
    , Problem.bpTopicId problem
    , statusId
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
          (Figure.bfName figure, SQL.Binary (Figure.bfContents figure), problemId)
      getProblemById conn problemId

-- | Delete a problem and its figures (on delete cascade should be set in schema).
deleteProblem
  :: SQL.Connection
  -> Integer -- ^ Problem ID
  -> IO () -- ^ No error handling
deleteProblem conn problemId = void $ SQL.execute conn
  "DELETE FROM problems WHERE id = ?" (SQL.Only problemId)

getProblemSets :: SQL.Connection -> Route.Query -> IO [ProblemSet.ProblemSet]
getProblemSets conn routeQuery = do
  let authorExpr = exprFromRouteParam
        "author"
        "author_id = ?"
        (id :: Integer -> Integer)
        routeQuery
  let exprs = [authorExpr]
  let whereClause = mconcat . intersperse " AND " . map fst . catMaybes $ exprs
  let whereParams = map snd . catMaybes $ exprs
  dbProblemSets :: [DbProblemSet.ProblemSet] <-
    if not . all isNothing $ exprs
    then SQL.query conn ("SELECT * FROM problem_sets WHERE " <> whereClause) whereParams
    else SQL.query_ conn "SELECT * FROM problem_sets"
  problemSetAuthors :: [User.User] <- sequence (map (fetchProblemSetAuthor conn) dbProblemSets)
  -- problemSetProblems :: [[Problem.Problem]]
  return
    $ flip map
    (zip
      dbProblemSets
      problemSetAuthors
    )
    $ \(dbProblemSet, problemSetAuthor) ->
        ProblemSet.ProblemSet
        { ProblemSet.id = DbProblemSet.id dbProblemSet
        , ProblemSet.summary = DbProblemSet.summary dbProblemSet
        , ProblemSet.author = problemSetAuthor
        , ProblemSet.createdAt = DbProblemSet.created_at dbProblemSet
        , ProblemSet.updatedAt = DbProblemSet.updated_at dbProblemSet
        -- , ProblemSet.problems = _
        }

getProblemsByProblemSetId :: SQL.Connection -> Integer -> IO [Problem.Problem]
getProblemsByProblemSetId conn problemSetId = do
  dbProblems :: [DbProblem.Problem] <- SQL.query conn "SELECT problems.* FROM problems INNER JOIN problem_set_problems ON problems.id = problem_id WHERE problem_set_id = ?" (SQL.Only problemSetId)
  let problemIds :: [Integer] = map DbProblem.id dbProblems
  mProblems :: [Maybe Problem.Problem] <- sequence $ map (getProblemById conn) problemIds
  return $ catMaybes mProblems

fetchProblemSetProblems :: SQL.Connection -> DbProblemSet.ProblemSet -> IO [Problem.Problem]
fetchProblemSetProblems conn (DbProblemSet.ProblemSet {DbProblemSet.id=problemSetId}) =
  getProblemsByProblemSetId conn problemSetId

fetchProblemSetAuthor :: SQL.Connection -> DbProblemSet.ProblemSet -> IO User.User
fetchProblemSetAuthor conn (DbProblemSet.ProblemSet {DbProblemSet.author_id=authorId}) =
  getUserById conn authorId
  >>= return . fromMaybe
  (error $ "fetchProblemSetAuthor: User not found with ID: " ++ show authorId)

getProblemSetById :: SQL.Connection -> Integer -> IO (Maybe ProblemSet.ProblemSet)
getProblemSetById conn problemSetId = do
  mDbProblemSet <- headMay
    <$> SQL.query conn "SELECT * FROM problem_sets WHERE id = ?" (SQL.Only problemSetId)
  case mDbProblemSet of
    Nothing -> return Nothing
    Just dbProblemSet -> do
      problems <- fetchProblemSetProblems conn dbProblemSet
      problemSetAuthor <- fetchProblemSetAuthor conn dbProblemSet
      return . Just $
        ProblemSet.ProblemSet
        { ProblemSet.id = DbProblemSet.id dbProblemSet
        , ProblemSet.summary = DbProblemSet.summary dbProblemSet
        , ProblemSet.author = problemSetAuthor
        , ProblemSet.createdAt = DbProblemSet.created_at dbProblemSet
        , ProblemSet.updatedAt = DbProblemSet.updated_at dbProblemSet
        , ProblemSet.problems = problems
        }

createProblemSet :: SQL.Connection -> ProblemSet.BareProblemSet -> IO (Maybe ProblemSet.ProblemSet)
createProblemSet conn newProblemSet = do
  mProblemSetId :: Maybe (SQL.Only Integer) <- headMay
    <$> SQL.query conn
    "INSERT INTO problem_sets (summary, author_id) VALUES (?,?) returning id"
    ( ProblemSet.bpsSummary newProblemSet
    , ProblemSet.bpsAuthorId newProblemSet
    )
  case SQL.fromOnly <$> mProblemSetId of
    Nothing -> return Nothing
    Just problemSetId -> do
      let problemIds :: [Integer] = ProblemSet.bpsProblemIds newProblemSet
      let positions :: [Integer] = [1 ..]
      forM_ (zip problemIds positions) $ \(problemId, position) -> do
        void $ SQL.execute conn
          "INSERT INTO problem_set_problems (problem_id, problem_set_id, position) VALUES (?,?,?)"
          ( problemId
          , problemSetId
          , position
          )
      getProblemSetById conn problemSetId

-- TODO: insert into problem_set_problems
updateProblemSet :: SQL.Connection -> ProblemSet.BareProblemSet -> IO (Maybe ProblemSet.ProblemSet)
updateProblemSet conn problemSet = do
  mProblemSetId :: Maybe (SQL.Only Integer) <- headMay
    <$> SQL.query conn
    "UPDATE problem_sets SET (summary, updated_at) = (?, ?, ?, ?, DEFAULT) WHERE id = ? returning id"
    ( ProblemSet.bpsSummary problemSet
    , ProblemSet.bpsProblemSetId problemSet
    )
  case SQL.fromOnly <$> mProblemSetId of
    Nothing -> return Nothing
    Just problemSetId -> do
      void $ SQL.execute conn
        "DELETE FROM figures WHERE problem_set_id = ?"
        (SQL.Only problemSetId)
      getProblemSetById conn problemSetId

-- | Delete a problem set.
-- TODO: delete from problem_set_problems
deleteProblemSet
  :: SQL.Connection
  -> Integer -- ^ Problem ID
  -> IO () -- ^ No error handling
deleteProblemSet conn problemSetId = void $ SQL.execute conn
  "DELETE FROM problem_sets WHERE id = ?" (SQL.Only problemSetId)

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

updateTopic :: SQL.Connection -> Topic.Topic -> IO (Maybe Topic.Topic)
updateTopic conn topic = do
  mTopicId :: Maybe (SQL.Only Integer) <- headMay
    <$> SQL.query conn
    "UPDATE topics SET (name, parent_id) = (?, ?) WHERE id = ? returning id"
    ( Topic.name topic
    , Topic.parentId topic
    , Topic.id topic
    )
  case SQL.fromOnly <$> mTopicId of
    Nothing -> return Nothing
    Just topicId -> getTopicById conn topicId

createTopic :: SQL.Connection -> Topic.NewTopic -> IO (Maybe Topic.Topic)
createTopic conn newTopic = do
  mTopicId :: Maybe (SQL.Only Integer) <- headMay
    <$> SQL.query conn
    "INSERT INTO topics(name, parent_id) VALUES (?,?) returning id"
    ( Topic.ntName newTopic
    , Topic.ntParentId newTopic
    )
  case SQL.fromOnly <$> mTopicId of
    Nothing -> return Nothing
    Just topicId -> do
      getTopicById conn topicId

deleteTopic
  :: SQL.Connection
  -> Integer -- ^ Topic ID
  -> IO () -- ^ No error handling
deleteTopic conn topicId = void $ SQL.execute conn
  "DELETE FROM topics WHERE id = ?" (SQL.Only topicId)

getRoles :: SQL.Connection -> IO [Role.Role]
getRoles conn = do
  dbRoles <- SQL.query_ conn "SELECT * FROM roles"
  return $ dbRoles <&> (\(DbRole.Role {DbRole.name=roleName}) -> read . cs $ roleName)

getRoleById :: SQL.Connection -> Integer -> IO (Maybe Role.Role)
getRoleById conn roleId = do
  mDbRole :: Maybe DbRole.Role <- headMay
    <$> SQL.query conn "SELECT * FROM roles WHERE id = ?" (SQL.Only roleId)
  return $ flip fmap mDbRole $
    \(DbRole.Role {DbRole.name=roleName}) -> read . cs $ roleName

getProblemStatusById :: SQL.Connection -> Integer -> IO (Maybe ProblemStatus.Status)
getProblemStatusById conn statusId = do
  mDbStatus :: Maybe DbProblemStatus.Status <- headMay
    <$> SQL.query conn "SELECT * FROM problem_statuses WHERE id = ?" (SQL.Only statusId)
  return $ flip fmap mDbStatus $
    \(DbProblemStatus.Status {DbProblemStatus.name=statusName}) -> read . cs $ statusName

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
  -- Delete any current rows for the same user
  void $ SQL.execute conn "DELETE FROM email_verifications WHERE user_id = ?"
    (SQL.Only userId)
  void $ SQL.execute conn
    "INSERT INTO email_verifications(secret, user_id) VALUES (?,?)"
    (secret, userId)
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
    , Figure.contents = SQL.fromBinary $ DbFigure.contents dbFigure
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
      , Figure.contents = SQL.fromBinary $ DbFigure.contents dbFigure
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

-- | Get all the meta settings.
getMetaSettings :: SQL.Connection -> IO [MetaSetting.MetaSetting]
getMetaSettings conn = do
  dbMetaSettings <- SQL.query_ conn "SELECT * FROM meta_settings"
  return
    $ dbMetaSettings
    <&>
    (\x -> MetaSetting.MetaSetting
      { MetaSetting.setting = read . cs $ DbMetaSetting.meta_setting x
      , MetaSetting.value = fromMaybe "" $ DbMetaSetting.meta_value x
      })

-- | Get a meta setting.
getMetaSetting
  :: SQL.Connection
  -> MetaSetting.Setting
  -> IO (Maybe MetaSetting.MetaSetting)
getMetaSetting conn setting = do
  mDbMetaSetting <- headMay
    <$> SQL.query conn
    "SELECT * FROM meta_settings WHERE meta_setting = ?"
    (SQL.Only $ show setting)
  case mDbMetaSetting of
    Nothing -> return Nothing
    Just x -> return . Just $ MetaSetting.MetaSetting
      { MetaSetting.setting = read . cs $ DbMetaSetting.meta_setting x
      , MetaSetting.value = fromMaybe "" $ DbMetaSetting.meta_value x
      }

-- | Safely set a meta setting, making sure it exists before updating it. Returns the updated setting.
setMetaSetting
  :: SQL.Connection
  -> MetaSetting.MetaSetting
  -> IO (Maybe MetaSetting.MetaSetting)
setMetaSetting conn metaSetting = do
  mDbMetaSetting :: Maybe DbMetaSetting.MetaSetting <- headMay
    <$> SQL.query conn "SELECT * FROM meta_settings WHERE meta_setting = ?"
    (SQL.Only $ show $ MetaSetting.setting metaSetting)
  case mDbMetaSetting of
    Nothing -> return Nothing
    Just _ -> do
      void $ SQL.execute conn
        "UPDATE meta_settings SET meta_value = ? WHERE meta_setting = ?"
        (MetaSetting.value metaSetting, show $ MetaSetting.setting metaSetting)
      mDbMetaSetting' :: Maybe DbMetaSetting.MetaSetting <- headMay <$> SQL.query conn
        "SELECT * FROM meta_settings WHERE meta_setting = ?"
        (SQL.Only $ show $ MetaSetting.setting metaSetting)
      case mDbMetaSetting' of
        Nothing -> return Nothing
        Just x -> return . Just $ MetaSetting.MetaSetting
          { MetaSetting.setting = read . cs $ DbMetaSetting.meta_setting x
          , MetaSetting.value = fromMaybe "" $ DbMetaSetting.meta_value x
          }
