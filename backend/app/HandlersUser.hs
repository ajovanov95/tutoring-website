{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HandlersUser where

import Data

import Servant
import Network.Wai

import Control.Monad(mapM)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.Reader(ReaderT)

import Database.Persist(selectList, entityVal, SelectOpt(LimitTo, Desc))
import Database.Persist.Sqlite(runSqlite, SqlBackend)

import Data.Time
import Data.Maybe(fromJust, maybe)
import Data.List(groupBy)

import System.Process(callProcess)

makeNewsGroups :: [News] -> [NewsGroup]
makeNewsGroups news =
    let
        year  = fromIntegral . (\(y, _, _) -> y) . toGregorian . utctDay . newsDateCreated
        month = (\(_, m, _) -> m) . toGregorian . utctDay . newsDateCreated
        
        grps1 = groupBy (\a b -> (year a == year b) && (month a == month b)) news
        grps2 = fmap (\grp -> NewsGroup (year $ head grp) (month $ head grp) grp) grps1
    in
        grps2

handlerNews :: Maybe Int -> Handler [NewsGroup]
handlerNews limit = 
    let
        selectOptions = (maybe [] ((:[]) . LimitTo) limit) ++ [Desc NewsDateCreated]

        getNewsAction :: (MonadIO m) => ReaderT SqlBackend m [NewsGroup]
        getNewsAction = 
            selectList [] selectOptions >>= 
                return . makeNewsGroups . fmap entityVal
    in
        (liftIO $ runSqlite "database.db" getNewsAction) >>= return

handlerEmail :: Email -> Handler String
handlerEmail Email{..} = do
    liftIO $ do 
        writeFile "/tmp/mailbody.txt" messageContent
        callProcess "/usr/bin/sendemail" args
        writeFile "/tmp/mailbody.txt" ""

    return "Вашата порака е успешно пратена."

    where
        args = ["-f", "aleksandar.jovanov.automail@gmail.com",
                "-u", "Baranje casovi za " ++ receiverAddress,
                "-t", "aleksandar.jovanov.1995@gmail.com",
                "-s", "smtp.gmail.com:587",
                "-o", "tls=yes",
                "-xu", "aleksandar.jovanov.automail@gmail.com",
                "-xp", "RwtskNsTkfEf7PMDWWtLsmeKj2otqjhttpJ7FApg3TNJ9DEdPxSrBsKhA4hRDcfa",
                 "-o", "message-file=/tmp/mailbody.txt"]

redirectHome :: Handler String
redirectHome = 
    throwError $ err301 { errHeaders = [("Location", "/index.html")] }