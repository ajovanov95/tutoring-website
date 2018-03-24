{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module HandlersAdministration where

import Data

import Servant

import Control.Exception(try)

import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.Reader(ReaderT)

import Database.Persist(insert, Key)
import Database.Persist.Sqlite(runSqlite, SqlBackend)
import Database.Sqlite(SqliteException)

handlerInsertNews :: News -> Handler String
handlerInsertNews news = do
    let insertAction = insert news :: (MonadIO m) => ReaderT SqlBackend m (Key News)
    let tryAction = (try $ runSqlite "database.db" insertAction) :: IO (Either SqliteException (Key News))
    possiblyKey <- liftIO tryAction
    case possiblyKey of
        Left e -> return $ "Error happened. " ++ (show e)
        Right key -> return $ "Successfully added to database. Key is " ++ (show key)