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
import System.Directory

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
        print $ "Running the server on port " ++ show port
        cwd  <- getCurrentDirectory
        print $ "Running in " ++ cwd
        print $ "Other files in this directory are: "
        getDirectoryContents cwd >>= print

        -- Run the server
        run port app
    where port = 8000