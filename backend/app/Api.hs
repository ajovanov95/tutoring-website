{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Api where

import Data

import Servant

type NewsApi       = "news"        :> QueryParam "limit" Int :> Get  '[JSON] [NewsGroup]

type InsertNewsApi = "insert-news" :> Capture "token" Token :> ReqBody '[JSON] News :> Post '[PlainText] String

type SendEmailApi  = "send-mail"   :> ReqBody '[JSON] Email  :> Post '[JSON] String

type RequestTokenApi = "request-token" :> Post '[JSON] ()

-- These are only used for redirects
type RedirectAdminApi = "admin" :> Get '[PlainText] String

type RedirectHomeApi = Get '[PlainText] String

type WholeApi = NewsApi :<|> 
                InsertNewsApi :<|> 
                SendEmailApi :<|> 
                RequestTokenApi :<|>
                RedirectHomeApi :<|> 
                RedirectAdminApi :<|>
                Raw

wholeApi :: Proxy WholeApi
wholeApi = Proxy