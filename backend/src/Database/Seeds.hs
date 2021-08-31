{-# LANGUAGE QuasiQuotes #-}

module Database.Seeds where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SqlQQ
import qualified Text.RawString.QQ as QQ

import qualified Util
import Global

load :: SQL.Connection -> IO ()
load conn = do
  password <- Util.hashPassword "123"

  let
    roleUser = (1, "User")
    roleContributor = (2, "Contributor")
    roleModerator = (3, "Moderator")
      :: (Integer, Text)
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      roles(id, name)
    VALUES
      (?,?)
  |]
    [roleUser, roleContributor, roleModerator]

  let
    userAlice = (1, "Alice", "alice@email.com", password, 3)
    userBob = (2, "Bob", "bob@email.com", password, 2)
    userCarol = (3, "Carol", "carol@email.com", password, 1)
      :: (Integer, Text, Text, Text, Integer)
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      users(id, full_name, email, password, role_id)
    VALUES
      (?,?,?,?,?)
  |]
    [ userAlice
    , userBob
    , userCarol
    ]

  let
    topicAstronomy = (1, "Astronomy", Nothing)
    topicBiology = (2, "Biology", Nothing)
    topicChemistry = (3, "Chemistry", Nothing)
    topicElectricalEngineering = (4, "Electrical Engineering", Nothing)
    topicMathematics = (5, "Mathematics", Nothing)
    topicPhysics = (6, "Physics", Nothing)
    topicPsychology = (7, "Psychology", Nothing)
    topicStatistics = (8, "Statistics", Nothing)
    topicAnalogCircuits = (9, "Analog Circuits", Just 4)
    topicCalculus = (10, "Calculus", Just 5)
    topicGroupTheory = (11, "Group Theory", Just 5)
    topicDifferentialEquations = (12, "Differential Equations", Just 10)
    topicLimits = (13, "Limits", Just 10)
    topicRates = (14, "Rates", Just 10)
      :: (Integer, Text, Maybe Integer)
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      topics(id, name, parent_id)
    VALUES
      (?,?,?)
  |]
    [ topicAstronomy
    , topicBiology
    , topicChemistry
    , topicElectricalEngineering
    , topicMathematics
    , topicPhysics
    , topicPsychology
    , topicStatistics
    , topicAnalogCircuits
    , topicCalculus
    , topicGroupTheory
    , topicDifferentialEquations
    , topicLimits
    , topicRates
    ]

  let
    problemCalculusCompoundRate =
      ( 1
      , "Find the present value of a continuous annuity at an annual rate of 2% compounded continuously for 4 years if the payment at time t is at the rate of $400 per year."
      , prbCalculusCompoundRate
      , topicRates^._1
      , userBob^._1
      , "now"
      , "now"
      )
    problemCurrMirror01 =
      ( 2
      , "Consider the wide-swing current mirror shown below where the desired output current is 30 uA. Given that M1 and M2 are identical in size and the minimum output voltage is 0.5V, find the length of the transistors such that the current mirror output resistance is 60 MOhm."
      , prbCurrMirror01
      , topicAnalogCircuits^._1
      , userAlice^._1
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
    [ problemCalculusCompoundRate
    , problemCurrMirror01
    ]

  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      figures(id, url, problem_id)
    VALUES
      (?,?,?)
  |]
    [ (1, "http://filehost.com/currMirror01a.asc", problemCurrMirror01^._1)
      :: (Integer, Text, Integer)
    ]

  let
    problemSetCalculusRates =
      ( 1
      , "Grade 12 calculus rates practice exercises"
      , topicRates^._1
      , userBob^._1
      , "now"
      , "now"
      )
      :: (Integer, Text, Integer, Integer, String, String)
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      problem_sets(id, summary, topic_id, author_id, created_at, updated_at)
    VALUES
      (?,?,?,?,?,?)
  |]
    [ problemSetCalculusRates
    ]

  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      problem_set_problems(problem_id, problem_set_id)
    VALUES
      (?,?)
  |]
    [ (problemCalculusCompoundRate^._1, problemSetCalculusRates^._1)
      :: (Integer, Integer)
    ]

prbCalculusCompoundRate :: Text
prbCalculusCompoundRate = [QQ.r|
\runParam{paramFormat = decimal}
\runParam{paramSigDigits = 7}

\runParam{R = 2;8;1}
\runParam{P = 400;800;100}
\runParam{Y = [4,5,6]}

\question Find the present value of a continuous annuity at an annual rate of
\val{R}\% compounded continuously for \val{Y} years if the payment at
time $t$ is at the rate of \$\val{P} per year.
\\
\\
\runSilent{Rdec = R/100}\\
\runSilent{PV = (P/(-Rdec))*(exp(-Y*Rdec) - 1)}\\
\begin{solutions}
    \textbf{Solution}\\
    \begin{align*}
    P.V. &= \int_{0}^{T} f(t)e^{-rt}dt\\
    &= \int_{0}^{\val{Y}} \val{P}e^{-\val{Rdec}t}dt\\
    &= \frac{\val{P}}{-\val{Rdec}}e^{-\val{Rdec}t} \Bigr|_{0}^\val{Y}\\
    &= \frac{\val{P}}{-\val{Rdec}}(e^{-\val{Y*Rdec}}-1)\\
    &= \$\val{PV}
    \end{align*}
\end{solutions}
\begin{answers}
    \textbf{Answer}\\
    P.V. = \$\val{PV}
\end{answers}
|]

prbCurrMirror01 :: Text
prbCurrMirror01 = [QQ.r|
\runParam{I_D = [30, 20, 10, 40] #\paramUnits{\mu A} }
\runParam{V_omin = [0.5, 0.4, 0.3, 0.6] #\paramLatex{V_{o,min}}}
\runParam{R_out = [60, 72, 80, 50]#\paramUnits{M \Omega}}
\runParam{V_tn = [0.3, 0.25, 0.35, 0.4] }
\runParam{unCox = [160, 250, 300, 200]# \paramUnits{\mu A/V^2} \paramLatex{\mu_n C_{ox}}}
\runParam{lambn = [0.04, 0.025, 0.05, 0.03]#\paramUnits{\mu m/V} \paramLatex{\lambda_n'}}

\question[6] Consider the wide-swing current mirror shown below where the desired output current is  \valU{I_D}.  Given that $M_1$ and $M_2$ are identical in size and the minimum output voltage is \valU{V_omin}, find the length of the transistors such that the current mirror output resistance is \valU{R_out}.

NMOS: $\val={V_tn}; \val={unCox}; \val={lambn}$

\begin{flushleft}
 \fontsize{12}{10}\selectfont
 \vspace{-20pt}
 \hbox{ \hspace{-100pt}  \incProbLTspice[1.0\textwidth]{currMirror01a}}
 \vspace{-40pt}
\end{flushleft}

\begin{blankSpace}
\vspace{100pt}
\end{blankSpace}

\begin{solutions}
\textbf{Solution}

\val={V_omin} $= 2V_{ov}$; \run={V_ov = V_omin/2} for both transistors\\
\run()={g_m = 2*I_D/V_ov} for each transistor\\
$R_{out}\approx g_mr_o^2$; \run()={r_o = sqrt(R_out/g_m)}\\
$r_o = L/(|\lambda_n'|I_D)$\\
\run()={L = r_o*abs(lambn)*I_D#\paramUnits{m}}\\
\hlite{\val={L}}

\end{solutions}

\begin{answers}
\textbf{Answer}

L = \valU{L}
\end{answers}
|]
