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
  FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp
);

CREATE TABLE IF NOT EXISTS email_verifications (
  id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
  secret TEXT NOT NULL UNIQUE,
  user_id BIGINT NOT NULL,
  FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp
);

CREATE TABLE IF NOT EXISTS reset_password (
  id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
  secret TEXT NOT NULL UNIQUE,
  user_id BIGINT NOT NULL,
  FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp
);

CREATE TABLE IF NOT EXISTS topics (
  id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
  name TEXT NOT NULL UNIQUE,
  parent_id BIGINT,
  FOREIGN KEY(parent_id) REFERENCES topics(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS problem_statuses (
  id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS problems (
  id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
  summary TEXT NOT NULL,
  contents TEXT NOT NULL,
  topic_id BIGINT NOT NULL,
  FOREIGN KEY(topic_id) REFERENCES topics(id),
  author_id BIGINT NOT NULL,
  FOREIGN KEY(author_id) REFERENCES users(id),
  status_id BIGINT NOT NULL,
  FOREIGN KEY(status_id) REFERENCES problem_statuses(id),
  created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp
);

CREATE TABLE IF NOT EXISTS figures (
  id BIGINT PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
  name TEXT NOT NULL,
  contents BYTEA NOT NULL,
  problem_id BIGINT NOT NULL,
  FOREIGN KEY(problem_id) REFERENCES problems(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp
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