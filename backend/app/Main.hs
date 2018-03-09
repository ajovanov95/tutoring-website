{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp(run)

import Database.Persist
import Database.Persist.Sqlite

import Servant

import System.Environment

import Data
import Api
import Handlers

server :: Server WholeApi
server = handlerNews  :<|> 
         handlerEmail :<|> 
         redirectHome :<|>
         (serveDirectoryWebApp ".") -- this will serve the static files

app :: Application 
app = wholeApi `serve` server

main :: IO ()
main = do
    args <- getArgs

    if "migrate" `elem` args then
        runSqlite "database.db" (runMigration migrateAll)
    else if "test-data" `elem` args then
        runSqlite "database.db" insertDummyNewsToDb
    else do 
        -- Run sqlite migrations
        print $ "Running the server on port " ++ show port
        -- Run the server
        run port app
    where port = 8000