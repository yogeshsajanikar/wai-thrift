{-# LANGUAGE OverloadedStrings #-}
module Main where

import GreetingApp
import qualified Greeting as G
import Network.Wai.Middleware.Cors
import Thrift.Protocol.JSON
import Thrift.Transport.Wai
import Network.Wai.Handler.Warp

-- | Sample application that runs warp, and handles thrift protocol
-- To test it use `curl -X POST -d "[1, \"hello\", 1, 0, {}]" http://localhost:3000`

main :: IO ()
main = run 3000 $ simpleCors $ thriftApp GreetData JSONProtocol JSONProtocol G.process

