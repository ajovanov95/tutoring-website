{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp(run)

import Servant

import Api
import Handlers

server :: Server WholeApi
server = handlerNews  :<|> 
         handlerEmail :<|> 
         redirectHome :<|>
         (serveDirectoryWebApp ".") -- this will serve the static files

app :: Application 
app = wholeApi `serve` server

main :: IO ()
main = run 8000 app