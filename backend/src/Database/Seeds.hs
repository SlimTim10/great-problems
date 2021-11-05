{-# LANGUAGE QuasiQuotes #-}

module Database.Seeds where

import Common.Lib.Prelude
import qualified Backend.Lib.Util as Util
import qualified Common.Api.Role as Role

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SqlQQ
import qualified Text.RawString.QQ as QQ

load :: SQL.Connection -> IO ()
load conn = do
  let roles :: [Role.Role] = [minBound ..]
  let rolesWithId :: [(Integer, Text)] = zip [1..] . map (cs . show) $ roles
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
    [ topicElectricalEngineering
    , topicMathematics
    , topicPhysics
    , topicAnalogCircuits
    , topicCalculus
    , topicDifferentialEquations
    , topicLimits
    , topicRates
    ]

  let problemCalculusDemo =
        ( 1
        , summaryCalculusDemo
        , prbCalculusDemo
        , topicCalculus^._1
        , userGreatProblems^._1
        , "now"
        , "now"
        )
        :: ( Integer
           , Text
           , Text
           , Integer
           , Integer
           , String
           , String
           )
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      problems(id, summary, contents, topic_id, author_id, created_at, updated_at)
    VALUES
      (?,?,?,?,?,?,?)
  |]
    [ problemCalculusDemo
    ]

  -- Fix sequences for auto-incrementing IDs
  _ <- SQL.query_ conn [SqlQQ.sql|
    SELECT setval(pg_get_serial_sequence('roles', 'id'), COALESCE((SELECT MAX(id)+1 FROM roles), 1), false);
    SELECT setval(pg_get_serial_sequence('users', 'id'), COALESCE((SELECT MAX(id)+1 FROM users), 1), false);
    SELECT setval(pg_get_serial_sequence('email_verifications', 'id'), COALESCE((SELECT MAX(id)+1 FROM email_verifications), 1), false);
    SELECT setval(pg_get_serial_sequence('topics', 'id'), COALESCE((SELECT MAX(id)+1 FROM topics), 1), false);
    SELECT setval(pg_get_serial_sequence('problems', 'id'), COALESCE((SELECT MAX(id)+1 FROM problems), 1), false);
    SELECT setval(pg_get_serial_sequence('figures', 'id'), COALESCE((SELECT MAX(id)+1 FROM figures), 1), false);
    SELECT setval(pg_get_serial_sequence('problem_sets', 'id'), COALESCE((SELECT MAX(id)+1 FROM problem_sets), 1), false);
  |] :: IO [SQL.Only Int]

  return ()

summaryCalculusDemo :: Text
summaryCalculusDemo = "Find the base, growth rate, and value of an exponential function."

prbCalculusDemo :: Text
prbCalculusDemo = [QQ.r|
\runParam{paramFormat = decimal}
\runParam{paramSigDigits = 5}

% \runParam{t1 = 17;20;1}
% \runParam{ft1 = 80;90;0.1}
% \runParam{t2 = 21;24;1}
% \runParam{ft2 = 80;90;0.1}
\runParam{t1 = 20}
\runParam{ft1 = 88.2}
\runParam{t2 = 23}
\runParam{ft2 = 91.4}
\runParam{t3 = 25}

\question Suppose that $Q = f(t)$ is an exponential function of $t$. If $f(\val{t1}) = \val{ft1}$ and $f(\val{t2}) = \val{ft2}$:

(a) Find the base.

(b) Find the growth rate.

(c) Evaluate $f(\val{t3})$.
\\
\\
\begin{solutions}
    \textbf{Solution}\\
    \begin{tabbing}
    (a) \=Let $Q = Q_0a^t$.\\
    \>Substituting $t = \val{t1}$, $Q = \val{ft1}$ and $t = \val{t2}$, $Q = \val{ft2}$ gives two equations for $Q_0$ and $a$:\\
    \\
    \>$\val{ft1} = Q_0a^{\val{t1}}$ and $\val{ft2} = Q_0a^{\val{t2}}$.\\
    \\
    \>Dividing the two equations enables us to eliminate $Q_0$:\\
    \\
    \runSilent{aexp = t2 - t1}
    \>$\dfrac{\val{ft2}}{\val{ft1}} = \dfrac{Q_0a^{\val{t2}}}{Q_0a^{\val{t1}}} = a^{\val{aexp}}$.\\
    \\
    \>Solving for the base, $a$, gives\\
    \\
    \runSilent{a_ans = (ft2 / ft1)^(1 / aexp)}
    \>$a = \left( \dfrac{\val{ft2}}{\val{ft1}} \right)^{1/{\val{aexp}}} = \val{a_ans}$.
    \end{tabbing}
    \begin{tabbing}
    \runSilent{gr = a_ans - 1}
    \runSilent{gr_per = gr * 100}
    (b) \=Since $a = \val{a_ans}$, the growth rate is $\val{a_ans} - 1 = \val{gr} = \val{gr_per}$\%.
    \end{tabbing}
    \begin{tabbing}
    \runSilent{gr = a_ans - 1}
    \runSilent{gr_per = gr * 100}
    (c) \=We want to evaluate $f(\val{t3}) = Q_0a^{\val{t3}} = Q_0(\val{a_ans})^{\val{t3}}$. First we need to find $Q_0$ from the equation\\
    \\
    \>$\val{ft1} = Q_0(\val{a_ans})^{\val{t1}}$\\
    \\
    \runSilent{q0 = ft1 * (1 / (a_ans^t1))}
    \>Solving gives $Q_0 = \val{q0}$. Thus,\\
    \\
    \runSilent{evaluate_ans = q0 * (a_ans^t3)}
    \>$f(\val{t3}) = \val{q0}(\val{a_ans})^{\val{t3}} = \val{evaluate_ans}$.
    \end{tabbing}
\end{solutions}
\begin{answers}
    \textbf{Answer}\\
    (a) $\val{a_ans}$
    
    (b) $\val{gr_per}$\%
    
    (c) $\val{evaluate_ans}$
\end{answers}
|]
