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

type NewsApi      = "news"      :> QueryParam "limit" Int :> Get  '[JSON] [NewsGroup]
type SendEmailApi = "send-mail" :> ReqBody '[JSON] Email  :> Post '[JSON] (Maybe String)

-- Dummy return type
type RedirectApi = Get '[JSON] Int

type WholeApi = NewsApi :<|> SendEmailApi :<|> RedirectApi :<|> Raw

wholeApi :: Proxy WholeApi
wholeApi = Proxy