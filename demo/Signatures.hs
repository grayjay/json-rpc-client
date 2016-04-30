{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Signatures where

import Network.JsonRpc.Client

-- Create a Signature for each server-side method:
concatenateSig :: Signature (String ::: String ::: ()) String
concatenateSig = Signature "concatenate" ("x" ::: "y" ::: ())

incrementSig :: Signature () Int
incrementSig = Signature "increment" ()
