{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp(run)

import Servant
import Data.Aeson
import GHC.Generics

-- Here be liftIO
-- Handler = ExceptT (ServantErr IO)
import Control.Monad.Except

data News = News {
    title :: String,
    content :: String,
    dateCreated :: String
} deriving (Eq, Show, Generic)

instance ToJSON News
instance FromJSON News

data Email = Email {
    receiverAddress :: String,
    messageContent :: String
} deriving (Eq, Show, Generic)

instance ToJSON Email
instance FromJSON Email

type NewsApi      = "news"      :> QueryParam "limit" Int :> Get '[JSON] [News]
type SendEmailApi = "send-mail" :> ReqBody '[JSON] Email  :> Post '[JSON] (Maybe String)

-- Dummy return type
type RedirectApi = Get '[JSON] Int

dummyNews = [
    News "Test 1" "This is some message" "23.2.2018",
    News "Test 2" "This is another message" "24.2.2018",
    News "Test 3" "This is a third message" "25.2.2018"]

type WholeApi = NewsApi :<|> SendEmailApi :<|> RedirectApi :<|> Raw

wholeApi :: Proxy WholeApi
wholeApi = Proxy

handlerNews :: Maybe Int -> Handler [News]
handlerNews Nothing  = return dummyNews
handlerNews (Just l) = return $ take l dummyNews

-- Nothing means no error
handlerEmail :: Email -> Handler (Maybe String)
handlerEmail Email{..} = do 
    return $ Nothing

redirectHome :: Handler Int
redirectHome = throwError $ err301 { errHeaders = [("Location", "/index.html")] }

server :: Server WholeApi
server = handlerNews  :<|> 
         handlerEmail :<|> 
         redirectHome :<|>
         (serveDirectoryWebApp ".") -- this will serve the static files

app :: Application 
app = wholeApi `serve` server

main :: IO ()
main = run 8000 app