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
  -- * Request Transport
  RequestTransport,
  fromRequest,
  -- * Stream Transport for response
  StreamTransport,
  toStreamTransport,
  -- * Wai compatible application and middleware
  thriftStreamingBody,
  thriftApp,
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

-- | Transport layer based on 'Request'
-- This is a readonly transport layer. Write operations will fail.
data RequestTransport = RequestTransport Request ReadBuffer

-- | Creates RequestTransport from WAI request
-- Initilizes RequestTransport with a lazy request body from request
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


-- | StreamTransport is write-only transport layer for thrift 'Transport'
data StreamTransport = StreamTransport { writer :: Builder -> IO ()  -- ^ to append response to the builder
                                       , flusher :: IO ()            -- ^ to flush the response to IO
                                       }

-- | Create 'StreamTransport' from two parts, builder creating the chunk
-- and flush, to flush the chunk to response stream. This is very similar
-- to 'StreamingBody'
toStreamTransport :: (Builder -> IO () )  -- ^ Builder for building the chunks
                  -> IO ()                -- ^ Flush the content to the response stream
                  -> StreamTransport      -- ^ 'StreamTransport' for Wai
toStreamTransport = StreamTransport


instance Transport StreamTransport where

  tIsOpen = const $ return True

  tClose = const $ return ()

  tRead _ _ = fail "Read operation is not supported for response"

  tPeek _ = fail "Peek is not allowed for response buffers"

  tWrite st bs = writer st $ fromLazyByteString bs

  tFlush = flusher


-- | Creates a streaming body that processes the request, and responds by
-- calling thrift handler. It uses 'RequestTransport' for processing the thrift
-- input, and 'StreamTransport' for responding through 'StreamingBody'
--
-- This is a very useful function, and is in fact used implement 'thriftWaiApp'
-- and 'thriftMiddleware'
--
-- For example one can use 'thriftStreamingBody' with scotty as
--
-- >
-- > import qualified Greeting as G  -- Thrift generated code
-- > 
-- > thriftStream :: ActionM ()
-- > thriftStream = do
-- >     req <- request
-- >     stream $ thriftStreamingBody G.GreetData JSONProtocol JSONProtocol G.process req
-- >
thriftStreamingBody ::
  (Protocol ip, Protocol op)
  => h                                                            -- ^ Type supporting thrift generated interface
  -> (RequestTransport -> ip RequestTransport)                    -- ^ Input protocol selector for 'RequestTransport'
  -> (StreamTransport -> op StreamTransport)                      -- ^ Output protocol selector for 'StreamTransport'
  -> (h -> (ip RequestTransport, op StreamTransport) -> IO Bool)  -- ^ Thrift request handler
  -> Request                                                      -- ^ Wai request, to be embedded in 'RequestTransport'
  -> StreamingBody                                                -- ^ Wai 'StreamingBody'
thriftStreamingBody h isp osp proc_ req write flushstream = do
  inp <- isp <$> fromRequest req
  let out = osp (StreamTransport write flushstream)
  _ <- proc_ h (inp, out)
  return ()
  


-- | Wai compatible application.
-- This does not add the necessary headers. 
-- This does not add necessary headers for allowing cross origin requests
thriftApp ::
  (Protocol ip, Protocol op)
  => h                                                              -- ^ Type supporting thrift generated interface
  -> (RequestTransport -> ip RequestTransport)                      -- ^ Input protocol selector for 'RequestTransport'
  -> (StreamTransport -> op StreamTransport)                        -- ^ Output protocol selector for 'StreamTransport'
  -> (h -> (ip RequestTransport, op StreamTransport) -> IO Bool)    -- ^ Thrift request handler
  -> Application                                                    -- ^ Wai application
thriftApp h isp osp proc_ req responder = 
  responder $ Wai.responseStream status200 [] $ thriftStreamingBody h isp osp proc_ req
 
  
-- | Creates Wai middleware for the given handler
-- This does not add necessary headers for allowing cross origin requests
thriftMiddleware :: (Protocol ip, Protocol op)
                    => h                                                             -- ^ Type supporting thrift generated interface
                    -> (RequestTransport -> ip RequestTransport)                     -- ^ Input protocol selector for 'RequestTransport'
                    -> (StreamTransport -> op StreamTransport)                       -- ^ Output protocol selector for 'StreamTransport' 
                    -> (h -> (ip RequestTransport, op StreamTransport) -> IO Bool)   -- ^ Thrift request handler
                    -> Middleware                                                    -- ^ Wai middleware
thriftMiddleware h isp osp proc_ app req responder = app req $ \res ->
  case res of
      ResponseStream {} -> 
        if methodPost == requestMethod req then
          responder
          $ Wai.responseStream status200 (responseHeaders res)
          $ thriftStreamingBody h isp osp proc_ req
        else
          responder res

      _ -> responder res
    

