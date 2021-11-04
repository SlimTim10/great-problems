{-# LANGUAGE QuasiQuotes #-}

module Database.Seeds where

import Common.Lib.Prelude
import qualified Backend.Lib.Util as Util
import qualified Common.Api.Role as Role

import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SqlQQ
import qualified Text.RawString.QQ as QQ

load :: SQL.Connection -> IO ()
load conn = do
  password <- Util.hashPassword "123"

  let roles :: [Role.Role] = [minBound ..]
  let rolesWithId :: [(Integer, Text)] = zip [1..] . map (cs . show) $ roles
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      roles(id, name)
    VALUES
      (?,?)
  |]
    rolesWithId

  let
    userAlice = (1, "Alice", "alice@email.com", password, 4, True)
    userBob = (2, "Bob", "bob@email.com", password, 2, True)
    userCarol = (3, "Carol", "carol@email.com", password, 1, True)
      :: (Integer, Text, Text, Text, Integer, Bool)
  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      users(id, full_name, email, password, role_id, verified)
    VALUES
      (?,?,?,?,?,?)
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
    problemCalculusDemo =
      ( 1
      , summaryCalculusDemo
      , prbCalculusDemo
      , topicCalculus^._1
      , userAlice^._1
      , "now"
      , "now"
      )
    problemCalculusCompoundRate =
      ( 2
      , "Find the present value of a continuous annuity at an annual rate of 2% compounded continuously for 4 years if the payment at time t is at the rate of $400 per year."
      , prbCalculusCompoundRate
      , topicRates^._1
      , userBob^._1
      , "now"
      , "now"
      )
    problemCurrMirror01 =
      ( 3
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
    [ problemCalculusDemo
    , problemCalculusCompoundRate
    , problemCurrMirror01
    ]

  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      figures(id, name, contents, problem_id)
    VALUES
      (?,?,?,?)
  |]
    [ (1, "currMirror01a.asc", figCurrMirror01, problemCurrMirror01^._1)
      :: (Integer, Text, B.ByteString, Integer)
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

  -- Fix sequences for auto-incrementing IDs
  _ <- SQL.query_ conn [SqlQQ.sql|
    SELECT setval(pg_get_serial_sequence('roles', 'id'), COALESCE((SELECT MAX(id)+1 FROM roles), 1), false);
    SELECT setval(pg_get_serial_sequence('users', 'id'), COALESCE((SELECT MAX(id)+1 FROM users), 1), false);
    SELECT setval(pg_get_serial_sequence('topics', 'id'), COALESCE((SELECT MAX(id)+1 FROM topics), 1), false);
    SELECT setval(pg_get_serial_sequence('problems', 'id'), COALESCE((SELECT MAX(id)+1 FROM problems), 1), false);
    SELECT setval(pg_get_serial_sequence('figures', 'id'), COALESCE((SELECT MAX(id)+1 FROM figures), 1), false);
    SELECT setval(pg_get_serial_sequence('problem_sets', 'id'), COALESCE((SELECT MAX(id)+1 FROM problem_sets), 1), false);
  |] :: IO [SQL.Only Int]

  return ()

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

figCurrMirror01 :: B.ByteString
figCurrMirror01 = [QQ.r|
Version 4
SHEET 1 1120 680
WIRE 112 288 112 272
WIRE 208 288 112 288
WIRE 112 320 112 288
WIRE 48 352 32 352
WIRE 112 416 112 384
WIRE 48 448 32 448
WIRE 112 496 112 480
FLAG 112 496 0
FLAG 208 288 vo
IOPIN 208 288 In
FLAG 32 448 VB1
IOPIN 32 448 In
FLAG 32 352 VB2
IOPIN 32 352 In
SYMBOL svg/svg_nmos 48 400 R0
SYMATTR InstName M1
SYMBOL svg/svg_nmos 48 304 R0
SYMATTR InstName M2
SYMBOL svg_meas_I 160 256 R180
SYMATTR InstName X1
TEXT 152 216 Left 2 ;\\val={I_D}
LINE Normal 112 272 112 192 1
|]

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
