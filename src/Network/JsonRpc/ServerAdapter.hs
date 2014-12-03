{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             UndecidableInstances,
             TypeOperators #-}

-- | Convenience function for creating server-side methods from 'Signature's.
module Network.JsonRpc.ServerAdapter ( -- * Server Methods
                                       toServerMethod
                                     , ConvertParams ) where

import Network.JsonRpc.Server ( Method, MethodParams
                              , Parameter (..), toMethod, (:+:) (..))
import Network.JsonRpc.Client (Signature (..), (:::) (..))

-- | Relationship between the parameters in a 'Signature' ('p1')
--   and the parameters expected by 'toMethod' ('p2') for a given
--   RPC method.
class ConvertParams p1 p2 | p1 -> p2 where
    createServerParams :: p1 -> p2

instance ConvertParams () () where
    createServerParams = id

instance ConvertParams p1 p2 => ConvertParams (a ::: p1) (a :+: p2) where
    createServerParams (name ::: ps1) = Required name :+: createServerParams ps1

-- | Creates a method from the given signature and function.
--   The parameters of the resulting method match the order
--   and types of the parameters in the signature and are all 'Required'.
toServerMethod :: (ConvertParams p1 p2, MethodParams f p2 m r) => Signature p1 r -> f -> Method m
toServerMethod (Signature name ps) f = toMethod name f $ createServerParams ps
