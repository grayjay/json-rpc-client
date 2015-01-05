{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             UndecidableInstances,
             TypeOperators #-}

-- | Convenience function for creating server-side methods from
--   'Signature's with the package
--   <http://hackage.haskell.org/package/json-rpc-server json-rpc-server>.
module Network.JsonRpc.ServerAdapter ( -- * Server Methods
                                       toServerMethod
                                     , ConvertParams ) where

import Network.JsonRpc.Client( Signature (..), (:::) (..))
import Network.JsonRpc.Server( Method, MethodParams
                             , Parameter (..), toMethod, (:+:) (..))

-- | Creates a method from the given signature and function.
--   The parameters of the resulting method match the order
--   and types of the parameters in the signature and are all 'Required'.
toServerMethod :: (ConvertParams ps1 ps2, MethodParams f ps2 m r) => Signature ps1 r -> f -> Method m
toServerMethod (Signature name ps) f = toMethod name f $ toServerParams ps

-- | Relationship between the parameters in a 'Signature' ('ps1')
--   and the parameters expected by 'toMethod' ('ps2') for a given
--   RPC method.
class ConvertParams ps1 ps2 | ps1 -> ps2, ps2 -> ps1 where
    toServerParams :: ps1 -> ps2

instance ConvertParams () () where
    toServerParams = id

instance ConvertParams ps1 ps2 => ConvertParams (p ::: ps1) (p :+: ps2) where
    toServerParams (name ::: ps1) = Required name :+: toServerParams ps1
