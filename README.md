# wai-thrift -- wai transport mechanism for thrift

## Request Transport

Request transport is `in` transport, or _read-only_ transport. It
converts request body into thrift transport. 

## Response Transport

Response transport is an `out` transport or _write-only_
transport. Wai chunked response is used as an `out` transport for
thrift. 


