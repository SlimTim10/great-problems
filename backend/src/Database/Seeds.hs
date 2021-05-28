{-# LANGUAGE QuasiQuotes #-}

module Database.Seeds where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SqlQQ
import qualified Crypto.PasswordStore
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
    , (2, "Mathematics", Nothing)
    , (3, "Calculus", Just 2)
      :: (Integer, Text, Maybe Integer)
    ]
