{-# LANGUAGE QuasiQuotes #-}
module Database.Seeds where

import Common.Lib.Prelude

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SqlQQ
import qualified Database.PostgreSQL.Simple.Migration as SQLM

import qualified Common.Api.Role as Role
import qualified Common.Api.ProblemStatus as ProblemStatus
import qualified Common.Api.MetaSetting as MetaSetting

rolesMigration :: SQL.Connection -> IO SQLM.MigrationCommand
rolesMigration conn = do
  let roles :: [Role.Role] = [minBound ..]
  let rolesWithId :: [(Integer, Text)] =
        map
        (\x -> (fromIntegral . fromEnum $ x, cs . show $ x))
        roles
  query <- SQL.formatMany conn [SqlQQ.sql|
    INSERT INTO
      roles(id, name)
    VALUES
      (?,?)
    ON CONFLICT DO NOTHING
    |]
    rolesWithId
  return $ SQLM.MigrationScript "insert roles" query

problemStatusesMigration :: SQL.Connection -> IO SQLM.MigrationCommand
problemStatusesMigration conn = do
  let problemStatuses :: [ProblemStatus.Status] = [minBound ..]
  let psWithId :: [(Integer, Text)] =
        map
        (\x -> (fromIntegral . fromEnum $ x, cs . show $ x))
        problemStatuses
  query <- SQL.formatMany conn [SqlQQ.sql|
    INSERT INTO
      problem_statuses(id, name)
    VALUES
      (?,?)
    ON CONFLICT DO NOTHING
    |]
    psWithId
  return $ SQLM.MigrationScript "insert problem statuses" query

metaSettingsMigration :: SQL.Connection -> IO SQLM.MigrationCommand
metaSettingsMigration conn = do
  let metaSettings :: [MetaSetting.Setting] = [minBound ..]
  let msWithId :: [(Integer, Text)] =
        map
        (\x -> (fromIntegral . fromEnum $ x, cs . show $ x))
        metaSettings
  query <- SQL.formatMany conn [SqlQQ.sql|
    INSERT INTO
      meta_settings(id, meta_setting)
    VALUES
      (?,?)
    ON CONFLICT DO NOTHING
    |]
    msWithId
  return $ SQLM.MigrationScript "insert meta settings" query
