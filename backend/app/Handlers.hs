{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Handlers where

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

-- Nothing means no error
handlerEmail :: Email -> Handler (Maybe String)
handlerEmail Email{..} = do 
    return $ Nothing

redirectHome :: Handler Int
redirectHome = throwError $ err301 { errHeaders = [("Location", "/index.html")] }