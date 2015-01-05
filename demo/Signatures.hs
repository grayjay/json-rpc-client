{-# LANGUAGE OverloadedStrings,
             TypeOperators #-}

module Signatures where

import Network.JsonRpc.Client

concatenateSig :: Signature (String ::: String ::: ()) String
concatenateSig = Signature "concatenate" ("x" ::: "y" ::: ())

incrementSig :: Signature () Int
incrementSig = Signature "increment" ()
