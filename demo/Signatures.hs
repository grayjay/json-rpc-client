{-# LANGUAGE OverloadedStrings,
             TypeOperators #-}

module Signatures where

import Network.JsonRpc.Client

concatSig :: Signature (String ::: String ::: ()) String
concatSig = Signature "concat" ("x" ::: "y" ::: ())

incSig :: Signature () Int
incSig = Signature "inc" ()
