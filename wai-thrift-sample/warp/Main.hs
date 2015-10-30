{-# LANGUAGE OverloadedStrings #-}
module Main where

import GreetingApp
import qualified Greeting as G
import Network.Wai.Middleware.Cors
import Web.Scotty
import Thrift.Protocol.JSON
import Thrift.Transport.Wai
import Control.Monad.Trans
import Network.Wai.Middleware.Static


corsHeaders :: ActionM ()
corsHeaders = do
  setHeader "Content-Type" "application/vnd.apache.thrift.json; charset=utf-8"
  setHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
  setHeader "Access-Control-Allow-Origin" "*"
  return ()

thriftStream :: (Protocol ip, Protocol op)
  => h
  -> (RequestTransport -> ip RequestTransport)
  -> (StreamTransport -> op StreamTransport)
  -> (h -> (ip RequestTransport, op StreamTransport) -> IO Bool)
  -> ActionM ()
thriftStream h isp osp proc_ = do
  corsHeaders
  req <- request
  stream $ thriftStreamingBody h isp osp proc_ req


main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "web")
  get "" $ redirect "/index.html"
  options "/hello" corsHeaders
  post "/hello" $ thriftStream GreetData JSONProtocol JSONProtocol G.process 

