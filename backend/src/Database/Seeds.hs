{-# LANGUAGE QuasiQuotes #-}

module Database.Seeds where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SqlQQ
import qualified Crypto.PasswordStore
import qualified Text.RawString.QQ as QQ
import Global

load :: SQL.Connection -> IO ()
load conn = do
  password :: Text <- cs <$> Crypto.PasswordStore.makePassword "123" 17
  
  void $ SQL.execute_ conn [SqlQQ.sql|
    TRUNCATE TABLE users CASCADE;
    TRUNCATE TABLE topics CASCADE;
    TRUNCATE TABLE problems CASCADE;
    TRUNCATE TABLE figures CASCADE;
  |]

  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      users(id, display_name, email, password)
    VALUES
      (?,?,?,?)
  |]
    [ (1, "test user 1", "test1@email.com", password)
    , (2, "test user 2", "test2@email.com", password)
    , (3, "test user 3", "test3@email.com", password)
      :: (Integer, Text, Text, Text)
    ]

  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      topics(id, name, parent_id)
    VALUES
      (?,?,?)
  |]
    [ (1, "Electrical Engineering", Nothing)
    , (2, "Circuit Design", Just 1)
    , (3, "Mathematics", Nothing)
    , (4, "Calculus", Just 3)
      :: (Integer, Text, Maybe Integer)
    ]

  void $ SQL.executeMany conn [SqlQQ.sql|
    INSERT INTO
      problems(id, title, description, contents, thumnail_url, author_id, topic_id)
    VALUES
      (?,?,?,?,?,?,?)
  |]
    [
      ( 1
      , "Calculus Compound Rate"
      , Nothing
      , prbCalculusCompoundRate
      , "https://cdn2.thecatapi.com/images/dbk.jpg"
      , 1
      , 4
      )
      :: ( Integer
         , Text
         , Maybe Text
         , Text
         , Text
         , Integer
         , Integer
         )
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
