{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Data where

import Data.Aeson
import GHC.Generics

import Data.Time
import Data.Maybe(fromJust)

import Database.Persist 
import Database.Persist.TH
import Database.Persist.Sqlite(SqlBackend)

import Control.Monad(mapM, mapM_)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.Reader(ReaderT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
News
    title String
    content String
    dateCreated UTCTime
    deriving Show Eq Generic
|]

data NewsGroup = NewsGroup {
    year :: Int,
    month :: Int,
    news :: [News]
} deriving (Eq, Show, Generic)

instance ToJSON News
instance FromJSON News

instance ToJSON NewsGroup

data Email = Email {
    receiverAddress :: String,
    messageContent :: String
} deriving (Eq, Show, Generic)

instance FromJSON Email

dummyNews = 
    let 
        t0 = UTCTime (fromJust $ fromGregorianValid 2018 3 15) 0
        t1 = UTCTime (fromJust $ fromGregorianValid 2017 11 22) 0
    in    
        [
            News "Тест порака" "Здраво свет" t0,
            News "Друга порака" "Денеска е надвор убав сончев ден" t0,
            News "Трета порака" "После се оди на лабораториски." t1,
            News "Четврта порака" "Пред некој ден врнеше" t1
        ]

insertDummyNewsToDb :: (MonadIO m) => ReaderT SqlBackend m ()
insertDummyNewsToDb = mapM_ insert dummyNews
