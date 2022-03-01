{-# LANGUAGE QuasiQuotes #-}

module Database.Seeds where

import Common.Lib.Prelude
import qualified Backend.Lib.Util as Util
import qualified Common.Api.Role as Role
import qualified Common.Api.ProblemStatus as ProblemStatus

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SqlQQ
import qualified Text.RawString.QQ as QQ

load :: SQL.Connection -> IO ()
load conn = do
  let roles :: [Role.Role] = [minBound ..]
  let rolesWithId :: [(Integer, Text)] =
        map
        (\x ->
           (fromIntegral . fromEnum $ x, cs . show $ x)
        )
        $ roles
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      roles(id, name)
    VALUES
      (?,?)
  |]
    rolesWithId

  -- Our demo user, who authors the demo problems
  secret <- Util.generateRandomText
  password <- Util.hashPassword secret
  let userGreatProblems = (1, "Great Problems", "support@greatproblems.ca", password, 1, True)
        :: (Integer, Text, Text, Text, Integer, Bool)
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      users(id, full_name, email, password, role_id, verified)
    VALUES
      (?,?,?,?,?,?)
  |]
    [ userGreatProblems
    ]

  let
    topicExamples = (0, "Examples", Nothing)
    topicElectricalEngineering = (1, "Electrical Engineering", Nothing)
    topicMathematics = (2, "Mathematics", Nothing)
    topicPhysics = (3, "Physics", Nothing)
    topicAnalogCircuits = (4, "Analog Circuits", Just 1)
    topicCalculus = (5, "Calculus", Just 2)
    topicDifferentialEquations = (6, "Differential Equations", Just 5)
    topicLimits = (7, "Limits", Just 5)
    topicRates = (8, "Rates", Just 5)
      :: (Integer, Text, Maybe Integer)
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      topics(id, name, parent_id)
    VALUES
      (?,?,?)
  |]
    [ topicExamples
    , topicElectricalEngineering
    , topicMathematics
    , topicPhysics
    , topicAnalogCircuits
    , topicCalculus
    , topicDifferentialEquations
    , topicLimits
    , topicRates
    ]

  let problemStatuses :: [ProblemStatus.Status] = [minBound ..]
  let problemStatusesWithId :: [(Integer, Text)] =
        map
        (\x ->
           (fromIntegral . fromEnum $ x, cs . show $ x)
        )
        $ problemStatuses
  let getProblemStatusId :: ProblemStatus.Status -> Integer = \status
        -> fromMaybe 1
           . fmap fst
           $ find (\(_, status') -> status' == (cs . show $ status)) problemStatusesWithId
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      problem_statuses(id, name)
    VALUES
      (?,?)
  |]
    problemStatusesWithId

  let problemCalculusDemo =
        ( 1
        , summaryCalculusDemo
        , prbCalculusDemo
        , topicCalculus^._1
        , userGreatProblems^._1
        , fromIntegral $ getProblemStatusId ProblemStatus.Published
        , "now"
        , "now"
        )
        :: ( Integer
           , Text
           , Text
           , Integer
           , Integer
           , Integer
           , String
           , String
           )
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      problems(id, summary, contents, topic_id, author_id, status_id, created_at, updated_at)
    VALUES
      (?,?,?,?,?,?,?,?)
  |]
    [ problemCalculusDemo
    ]

  -- Fix sequences for auto-incrementing IDs
  _ <- SQL.query_ conn [SqlQQ.sql|
    SELECT setval(pg_get_serial_sequence('roles', 'id'), COALESCE((SELECT MAX(id)+1 FROM roles), 1), false);
    SELECT setval(pg_get_serial_sequence('users', 'id'), COALESCE((SELECT MAX(id)+1 FROM users), 1), false);
    SELECT setval(pg_get_serial_sequence('email_verifications', 'id'), COALESCE((SELECT MAX(id)+1 FROM email_verifications), 1), false);
    SELECT setval(pg_get_serial_sequence('reset_password', 'id'), COALESCE((SELECT MAX(id)+1 FROM reset_password), 1), false);
    SELECT setval(pg_get_serial_sequence('topics', 'id'), COALESCE((SELECT MAX(id)+1 FROM topics), 1), false);
    SELECT setval(pg_get_serial_sequence('problem_statuses', 'id'), COALESCE((SELECT MAX(id)+1 FROM problem_statuses), 1), false);
    SELECT setval(pg_get_serial_sequence('problems', 'id'), COALESCE((SELECT MAX(id)+1 FROM problems), 1), false);
    SELECT setval(pg_get_serial_sequence('figures', 'id'), COALESCE((SELECT MAX(id)+1 FROM figures), 1), false);
    SELECT setval(pg_get_serial_sequence('problem_sets', 'id'), COALESCE((SELECT MAX(id)+1 FROM problem_sets), 1), false);
  |] :: IO [SQL.Only Int]

  return ()

summaryCalculusDemo :: Text
summaryCalculusDemo = "Find the base, growth rate, and value of an exponential function."

prbCalculusDemo :: Text
prbCalculusDemo = [QQ.r|CONFIG{fmtVal=D3, fmtRun()=D3, fmtRunEQ=D3, verbose}
PARAM{t_1 = [20,19,18,17]}
PARAM{f_1 = 88.2;90;0.1}
PARAM{t_2 = [23,24,22,21]}
PARAM{f_2 = 91.4;99.9;0.1}
PARAM{t_3 = 25}
Suppose that $Q = f(t)$ is an exponential function of $t$. If $f(VAL{t_1}) = VAL{f_1}$ and $f(VAL{t_2}) = VAL{f_2}$

(a) Find the base.
(b) Find the growth rate.
(c) Evaluate $f(VAL{t_3})$.

\begin{solution}
(a) Let $Q = Q_0a^t$.
Substituting $t = VAL{t_1}$, $Q = VAL{f_1}$ and $t = VAL{t_2}$, $Q = VAL{f_2}$ gives two equations for $Q_0$ and $a$:
\begin{equation}
RUN{f_1=Q_0a^t_1#fmt=eqnVal} \text{ and } RUN{f_2=Q_0a^t_2#fmt=eqnVal}
\end{equation}  
Dividing the two equations enables us to eliminate $Q_0$:
\begin{equation}
RUN{t21 = t_2 - t_1#fmt=silent}
RUN{DIV(f_2,f_1)=DIV(Q_0a^t_2,Q_0a^t_1)=a^t21#fmt=eqnVal}
\end{equation}
Solving for the base, $a$, gives
\begin{equation}
RUN{a = (f_2 / f_1)^(1 / t21)#fmt=silent}
a=RUN{\left(DIV(f_2,f_1)\right)^{(1/t21)}=a#fmt=eqnVal}
\end{equation}
So the base is \hilite{VAL{a}}
(b) Since $VAL{a,=}$, the growth rate is $VAL{a} - 1 = VAL{a-1} \hilite{= VAL{(a-1)*100}\%}$

(c) We want to evaluate $f(VAL{t_3}) = Q_0a^{VAL{t_3}} = Q_0(VAL{a})^{VAL{t_3}}$. First we need to find $Q_0$ from the equation
\begin{equation}
RUN{f_1=Q_0(a)^t_1#fmt=eqnVal}
\end{equation}
Solving gives
\begin{equation}
RUN{Q_0 = f_1/(a^t_1)#fmt=silent}
Q_0 = RUN{DIV(f_1,(a)^t_1)=Q_0#fmt=eqnVal}
\end{equation}
Thus,
RUN{f_3=Q_0*a^t_3#fmt=silent}
\begin{equation}
f(VAL{t_3})=Q_0a^{VAL{t_3}} \hilite{= VAL{f_3}}
\end{equation}
\end{solution}
\begin{answer}
    (a) $VAL{a}$
    (b) $VAL{(a-1)*100}$\%
    (c) $VAL{f_3}$
\end{answer}|]
