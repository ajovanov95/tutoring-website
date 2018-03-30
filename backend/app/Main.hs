{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe(isJust, fromJust)

import Network.Wai
import Network.Wai.Handler.Warp(run)

import Database.Persist
import Database.Persist.Postgresql
import Data.ByteString.Char8(pack)

import Servant

import System.Environment
import System.Directory

import Data
import Api
import HandlersUser
import HandlersAdministration
import Control

server :: AppState -> Server WholeApi
server appState = 
    handlerNews appState :<|>
    handlerInsertNews appState :<|>
    handlerEmail appState :<|> 
    handlerRequestToken appState :<|>
    redirectHome :<|>
    redirectAdmin :<|>
    (serveDirectoryWebApp "static")

mkApp :: AppState -> Application 
mkApp appState = serve wholeApi (server appState)

main :: IO ()
main = do
    args <- getArgs
    maybePort <- lookupEnv "PORT"
    maybeDatabaseUrl <- lookupEnv "DATABASE_URL"

    if (not (isJust (maybePort >>= toInt)) || not (isJust maybeDatabaseUrl))
    then do 
        print $ "ERROR: I need both $PORT and $DATABASE_URL existing and set to valid values."
        print $ "Port is " ++ (show maybePort)
        print $ "Database url is " ++ (show maybeDatabaseUrl) 
    else do
        let databaseUrl = pack $ fromJust maybeDatabaseUrl
        if "migrate" `elem` args then
            runDatabase databaseUrl (runMigration migrateAll)
        else if "test-data" `elem` args then
            runDatabase databaseUrl insertDummyNewsToDb
        else do 
            appState <- mkAppState databaseUrl
            let port = fromJust $ maybePort >>= toInt;
            print $ "Running the server on port " ++ show port
            print $ "Comunnicating with database on url " ++ (show databaseUrl)
            runDatabase databaseUrl (runMigration migrateAll)
            run port (mkApp appState)
    where 
        toInt :: String -> Maybe Int
        toInt x = Just $ (read x :: Int)