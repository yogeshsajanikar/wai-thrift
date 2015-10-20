{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Thrift.Transport.Wai
Description : Wai support for thrift transport
License     : MIT
Maintainer  : Yogesh Sajanikar <yogesh_sajanikar@yahoo.com>
Stability   : experimental
Portability : POSIX, WINDOWS

Support thrift transport for Wai Request and Response.

-}
module Thrift.Transport.Wai (
  RequestTransport,
  StreamTransport,
  fromRequest,
  toStreamTransport,
  thriftWaiApp,
  thriftMiddleware
  ) where

import Thrift.Transport
import Thrift.Protocol
import Thrift.Transport.IOBuffer
import Network.Wai as Wai
import Network.Wai.Internal
import Data.IORef
import Blaze.ByteString.Builder
import Data.Monoid
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Method

data RequestTransport = RequestTransport Request ReadBuffer

-- | Creates RequestTransport from WAI request
fromRequest :: Request -> IO RequestTransport
fromRequest req = RequestTransport
                  <$> return req
                  <*> (Wai.lazyRequestBody req >>= newIORef)

instance Transport RequestTransport where

  tIsOpen = const $ return True

  tClose = const $ return ()

  tRead (RequestTransport _ b) n = readBuf b (fromIntegral n)

  tPeek (RequestTransport _ b) = peekBuf b

  tWrite _ _ = fail "RequestTransport does not support write"

  tFlush _ = fail "RequestTransport does not support flush"


type WriteBuilder = IORef Builder

newtype ResponseTransport = ResponseTransport WriteBuilder

newResponseTransport :: IO ResponseTransport
newResponseTransport = ResponseTransport <$> (newIORef mempty)

instance Transport ResponseTransport where

  tIsOpen = const $ return False

  tClose = const $ return ()

  tRead _ _ = fail "Read operation is not supported for response"

  tPeek _ = fail "Peek is not allowed for response buffers"

  tWrite (ResponseTransport b) bs = modifyIORef b (<> fromLazyByteString bs)

  tFlush (ResponseTransport b) = modifyIORef b (<> flush)
  


data StreamTransport = StreamTransport { writer :: Builder -> IO ()
                                       , flusher :: IO ()
                                       }

toStreamTransport :: (Builder -> IO () ) -> IO () -> StreamTransport
toStreamTransport w f = StreamTransport w f


instance Transport StreamTransport where

  tIsOpen = const $ return True

  tClose = const $ return ()

  tRead _ _ = fail "Read operation is not supported for response"

  tPeek _ = fail "Peek is not allowed for response buffers"

  tWrite st bs = writer st $ fromLazyByteString bs

  tFlush st = flusher st



thriftWaiApp ::
  (Protocol ip, Protocol op)
  => h
  -> (RequestTransport -> ip RequestTransport)
  -> (StreamTransport -> op StreamTransport)
  -> (h -> (ip RequestTransport, op StreamTransport) -> IO Bool)
  -> Application 
thriftWaiApp h isp osp proc_ req responder = do
  inp <- isp <$> fromRequest req
  responder $ Wai.responseStream status200 [] $ \write flushstream -> do
    let out = osp (StreamTransport write flushstream)
    _ <- proc_ h (inp, out)
    return ()
 
  
-- | Creates Wai middleware for the given handler
thriftMiddleware :: (Protocol ip, Protocol op)
                    => h
                    -> (RequestTransport -> ip RequestTransport)
                    -> (StreamTransport -> op StreamTransport)
                    -> (h -> (ip RequestTransport, op StreamTransport) -> IO Bool)
                    -> Application
                    -> Application
thriftMiddleware h isp osp proc_ app req responder = app req $ \res ->
  case res of
      ResponseStream {} -> 
        if methodPost == requestMethod req then
          do
            inp <- isp <$> fromRequest req
            responder $ Wai.responseStream status200 (responseHeaders res) $ \write flushstream -> do
              let out = osp (StreamTransport write flushstream)
              _ <- proc_ h (inp, out)
              return ()
        else
          responder res

      _ -> responder res
    

