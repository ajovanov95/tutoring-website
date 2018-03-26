{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module HandlersAdministration where

import Data

import Utilities

import Servant

import Control.Exception(try)

import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.Reader(ReaderT)

import Database.Persist(insert, Key)
import Database.Persist.Sqlite(runSqlite, SqlBackend)
import Database.Sqlite(SqliteException)

handlerInsertNews :: TokenStateVar -> Integer -> News -> Handler String
handlerInsertNews tokenStateVar token news = do
    realToken <- liftIO $ getToken tokenStateVar
    if realToken /= token
    then return "Tokens do not match. Access to database denied."
    else do 
        let insertAction = insert news :: (MonadIO m) => ReaderT SqlBackend m (Key News)
        let tryAction = (try $ runSqlite "database.db" insertAction) :: IO (Either SqliteException (Key News))
        possiblyKey <- liftIO tryAction
        case possiblyKey of
            Left e -> return $ "Error happened. " ++ (show e)
            Right key -> return $ "Successfully added to database. Key is " ++ (show key)

handlerRequestToken :: TokenStateVar -> Handler ()
handlerRequestToken tokenStateVar = 
    liftIO $ do 
        token <- getToken tokenStateVar
        sendEmail ("Tutoring website news editing access token") (mkMessage token)
    where 
        mkMessage token = "Your token is\t" ++ (show token)

redirectAdmin :: Handler String
redirectAdmin = redirect "/admin.html"