{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant
import Data.Aeson
import GHC.Generics

type SecondsSinceEpoch = Float

data News = News {
    title :: String,
    content :: String,
    dateCreated :: SecondsSinceEpoch
} deriving (Eq, Show, Generic)

data NewsGroup = NewsGroup {
    year :: Int,
    month :: Int,
    news :: [News]
} deriving (Eq, Show, Generic)

instance ToJSON News
instance ToJSON NewsGroup

data Email = Email {
    receiverAddress :: String,
    messageContent :: String
} deriving (Eq, Show, Generic)

instance FromJSON Email

type NewsApi      = "news"      :> QueryParam "limit" Int :> Get '[JSON] [NewsGroup]
type SendEmailApi = "send-mail" :> ReqBody '[JSON] Email  :> Post '[JSON] (Maybe String)

-- Dummy return type
type RedirectApi = Get '[JSON] Int

type WholeApi = NewsApi :<|> SendEmailApi :<|> RedirectApi :<|> Raw

wholeApi :: Proxy WholeApi
wholeApi = Proxy