{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Api 

import Servant
import Network.Wai

import Control.Monad.IO.Class(liftIO)

dummyNews = [
    NewsGroup 2015 3 
        [News "Test 1" "This is some message" 1,
        News "Test 2" "This is another message" 2,
        News "Test 3" "This is a third message" 3],
     NewsGroup 2015 1 [
         News "Test 4" "This is a fourth message" 4
     ]
    ]

handlerNews :: Maybe Int -> Handler [NewsGroup]
handlerNews Nothing  = do 
    return dummyNews

handlerNews (Just l) = do 
    return $ take l dummyNews

-- Nothing means no error
handlerEmail :: Email -> Handler (Maybe String)
handlerEmail Email{..} = do 
    return $ Nothing

redirectHome :: Handler Int
redirectHome = throwError $ err301 { errHeaders = [("Location", "/index.html")] }