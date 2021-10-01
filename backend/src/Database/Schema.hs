{-# LANGUAGE QuasiQuotes #-}

module Database.Schema where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SqlQQ
import Global

load :: SQL.Connection -> IO ()
load conn = void $ SQL.execute_ conn [SqlQQ.sql|
  CREATE TABLE IF NOT EXISTS roles (
    id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
    name TEXT NOT NULL UNIQUE
  );

  CREATE TABLE IF NOT EXISTS users (
    id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
    full_name CITEXT NOT NULL,
    email CITEXT NOT NULL UNIQUE,
    password TEXT NOT NULL,
    role_id BIGINT,
    FOREIGN KEY(role_id) REFERENCES roles(id),
    verified BOOLEAN DEFAULT FALSE
  );

  CREATE TABLE IF NOT EXISTS sessions (
    id TEXT NOT NULL UNIQUE,
    user_id BIGINT,
    FOREIGN KEY(user_id) REFERENCES users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp
  );

  CREATE TABLE IF NOT EXISTS email_verifications (
    id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
    secret TEXT NOT NULL UNIQUE,
    user_id BIGINT NOT NULL,
    FOREIGN KEY(user_id) REFERENCES users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp
  );

  CREATE TABLE IF NOT EXISTS topics (
    id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
    name TEXT NOT NULL UNIQUE,
    parent_id BIGINT,
    FOREIGN KEY(parent_id) REFERENCES topics(id)
  );

  CREATE TABLE IF NOT EXISTS problems (
    id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
    summary TEXT NOT NULL,
    content TEXT NOT NULL,
    topic_id BIGINT NOT NULL,
    FOREIGN KEY(topic_id) REFERENCES topics(id),
    author_id BIGINT NOT NULL,
    FOREIGN KEY(author_id) REFERENCES users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp
  );

  CREATE TABLE IF NOT EXISTS figures (
    id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
    url TEXT NOT NULL,
    problem_id BIGINT NOT NULL,
    FOREIGN KEY(problem_id) REFERENCES problems(id)
  );

  CREATE TABLE IF NOT EXISTS problem_sets (
    id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
    summary TEXT NOT NULL,
    topic_id BIGINT NOT NULL,
    FOREIGN KEY(topic_id) REFERENCES topics(id),
    author_id BIGINT NOT NULL,
    FOREIGN KEY(author_id) REFERENCES users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp
  );

  CREATE TABLE IF NOT EXISTS problem_set_problems (
    problem_id BIGINT NOT NULL,
    FOREIGN KEY(problem_id) REFERENCES problems(id),
    problem_set_id BIGINT NOT NULL,
    FOREIGN KEY(problem_set_id) REFERENCES problem_sets(id),
    PRIMARY KEY (problem_id, problem_set_id)
  );
  |]

unload :: SQL.Connection -> IO ()
unload conn = void $ SQL.execute_ conn [SqlQQ.sql|
  DROP TABLE IF EXISTS roles CASCADE;
  DROP TABLE IF EXISTS users CASCADE;
  DROP TABLE IF EXISTS sessions CASCADE;
  DROP TABLE IF EXISTS email_verifications CASCADE;
  DROP TABLE IF EXISTS topics CASCADE;
  DROP TABLE IF EXISTS problems CASCADE;
  DROP TABLE IF EXISTS figures CASCADE;
  DROP TABLE IF EXISTS problem_sets CASCADE;
  DROP TABLE IF EXISTS problem_set_problems CASCADE;
  |]
