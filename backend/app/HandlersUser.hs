{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HandlersUser where

import Data
import Control

import Control.Exception(try, SomeException)

import Servant
import Network.Wai

import Control.Monad(mapM)
import Control.Monad.IO.Class(liftIO, MonadIO)

import Database.Persist(selectList, entityVal, SelectOpt(LimitTo, Desc))
import Database.Persist.Sql(SqlPersistT)

import Data.Time
import Data.Maybe(fromJust, maybe)
import Data.List(groupBy)

makeNewsGroups :: [News] -> [NewsGroup]
makeNewsGroups news =
    let
        date  = toGregorian . utctDay . newsDateCreated
        year  = fromIntegral . (\(y, _, _) -> y) . date
        month = (\(_, m, _) -> m) . date
        
        grps1 = groupBy (\a b -> (year a == year b) && (month a == month b)) news
        grps2 = fmap (\grp -> NewsGroup (year $ head grp) (month $ head grp) grp) grps1
    in
        grps2

handlerNews :: AppState -> Maybe Int -> Handler [NewsGroup]
handlerNews AppState{..} limit = 
    let
        selectOptions = (maybe [] ((:[]) . LimitTo) limit) ++ [Desc NewsDateCreated]

        getNewsAction :: (MonadIO m) => SqlPersistT m [NewsGroup]
        getNewsAction = 
            selectList [] selectOptions >>= 
                return . makeNewsGroups . fmap entityVal
    in
        (liftIO $ runDatabase databaseUrl getNewsAction) >>= return

handlerEmail :: AppState -> Email -> Handler String
handlerEmail _ Email{..} = do
    res <- liftIO tryAction

    case res of
        Left e -> do 
            liftIO $ print $ "ERROR: " ++ (show e) 
            return $ "Дојде до грешка при праќање."
        Right _ -> return $ "Вашата порака е успешно пратена."
    where 
        title = "Baranje casovi za " ++ receiverAddress
        tryAction = (try $ sendEmail title messageContent) :: IO (Either SomeException ())

redirectHome :: Handler String
redirectHome = redirect "/index.html"
    