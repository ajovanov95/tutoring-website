{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies #-}

module Control (AppState(..),
                sendEmail, 
                redirect,
                getToken, 
                mkAppState,
                runDatabase) where

import Data

import Servant

import System.Process(callProcess)

import Data.ByteString.Internal

import Data.Time
import Data.Maybe(fromJust)

import Control.Concurrent
import Control.Concurrent.STM 

import System.Random(randomRIO)

import Control.Monad.IO.Class(liftIO, MonadIO)

import Control.Exception(Exception, throw)

import Control.Monad.Logger    (runStderrLoggingT)
import Database.Persist
import Database.Persist.Postgresql

type EmailSubject = String

data TokenState = TokenState {
    lastToken :: Integer,
    lastTime :: UTCTime,
    tokenValidityPeriod :: NominalDiffTime
} deriving (Show)

type TokenStateVar = TVar TokenState
type IsTokenFresh = Bool

data AppState = AppState {
    tokenStateVar :: TokenStateVar,
    databaseUrl :: ConnectionString
}

sendEmail :: EmailSubject -> MessageContent -> IO ()
sendEmail emailSubject messageContent = 
    do 
        writeFile "/tmp/mailbody.txt" messageContent
        callProcess "/usr/bin/sendemail" args
        writeFile "/tmp/mailbody.txt" ""
    where
        args = ["-f", "aleksandar.jovanov.automail@gmail.com",
                "-u", emailSubject,
                "-t", "aleksandar.jovanov.1995@gmail.com",
                "-s", "smtp.gmail.com:587",
                "-o", "tls=yes",
                "-xu", "aleksandar.jovanov.automail@gmail.com",
                "-xp", "RwtskNsTkfEf7PMDWWtLsmeKj2otqjhttpJ7FApg3TNJ9DEdPxSrBsKhA4hRDcfa",
                 "-o", "message-file=/tmp/mailbody.txt"]

redirect :: ByteString -> Handler String
redirect loc = throwError $ err301 { errHeaders = [("Location", loc)] }

inititalTokenState =
    let 
        start   = UTCTime (fromJust $ fromGregorianValid 1 1 1) 0
        fifteen = realToFrac $ 15 * 60
    in
        TokenState 0 start fifteen

getToken :: TokenStateVar -> IO (Token, IsTokenFresh)
getToken tokenStateVar = do
    time <- getCurrentTime
    newToken <- randomRIO (1, 1000000000000)
    atomically $ do 
        tokenState <- readTVar tokenStateVar

        let elapsed = diffUTCTime time (lastTime tokenState);
        let newTokenState = tokenState { lastToken = newToken, lastTime = time };

        if elapsed <= tokenValidityPeriod tokenState
        then return (lastToken tokenState, False)
        else do 
            writeTVar tokenStateVar newTokenState
            return (lastToken newTokenState, True)

mkAppState :: ConnectionString -> IO (AppState)
mkAppState databaseUrl = do
    tsv <- newTVarIO inititalTokenState

    return AppState {
            tokenStateVar = tsv,
            databaseUrl = databaseUrl };

-- Input type to this function
-- SqlPersistT (NoLoggingT (ResourceT IO)) a 
runDatabase databaseUrl actions = do
    runStderrLoggingT $ 
        withPostgresqlPool databaseUrl nConnections $ 
            \pool -> liftIO $ runSqlPersistMPool actions pool
        where 
            nConnections = 1