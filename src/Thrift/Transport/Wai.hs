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
module Thrift.Transport.Wai where

import Thrift.Transport
import Thrift.Transport.IOBuffer
import Network.Wai as Wai
import Data.IORef
import Blaze.ByteString.Builder
import Data.Monoid

data RequestTransport = RequestTransport { request :: Request   -- ^ WAI request
                                         , buffer :: ReadBuffer -- ^ Request body
                                         }

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
  
