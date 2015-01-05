json-rpc-client
===============
[![Build Status](https://travis-ci.org/grayjay/json-rpc-client.svg?branch=master)](https://travis-ci.org/grayjay/json-rpc-client)


An implementation of the client side of JSON-RPC 2.0.  See
http://www.jsonrpc.org/specification. This library supports
batch requests and notifications, as well as single method
calls.  It also provides a function for creating corresponding
server-side methods with the package [json-rpc-server]
(http://hackage.haskell.org/package/json-rpc-server).
This library does not handle transport, so a function for
communicating with the server must be provided.
