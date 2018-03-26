{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Utilities where

import Data

import Servant

import System.Process(callProcess)

import Data.ByteString.Internal

import Data.Time
import Data.Maybe(fromJust)

import Control.Concurrent
import Control.Concurrent.STM 

import System.Random(randomRIO)

import Control.Monad.IO.Class(liftIO)

type EmailSubject = String

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

data TokenState = TokenState {
    lastToken :: Integer,
    lastTime :: UTCTime,
    tokenValidityPeriod :: NominalDiffTime
} deriving (Show)

inititalTokenState =
    let 
        start   = UTCTime (fromJust $ fromGregorianValid 1 1 1) 0
        fifteen = realToFrac $ 15 * 60
    in
        TokenState 0 start fifteen

type TokenStateVar = TVar TokenState

makeTokenStateVar :: IO TokenStateVar
makeTokenStateVar = newTVarIO inititalTokenState

getToken :: TokenStateVar -> IO Token
getToken tokenStateVar = do
    time <- getCurrentTime
    newToken <- randomRIO (1, 1000000000000)
    atomically $ do 
        tokenState <- readTVar tokenStateVar

        let elapsed = diffUTCTime time (lastTime tokenState);
        let newTokenState = tokenState { lastToken = newToken, lastTime = time };

        if elapsed <= tokenValidityPeriod tokenState
        then return $ lastToken tokenState
        else do 
            writeTVar tokenStateVar newTokenState
            return $ lastToken newTokenState
