{-# LANGUAGE OverloadedStrings,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators #-}

module Network.JsonRpc.Server
    ( createServerFunction ) where

import Network.JsonRpc.Common
import Network.JsonRpc.Internal
import qualified Data.ByteString.Lazy as B

class Monad m => Server f ps r m | m ps r -> f where
    createServerFunction :: (Server f ps r m) => Signature ps r -> f -> B.ByteString -> m B.ByteString

instance Monad m => Server (RpcResult m a) () a m
instance Server f ps r m => Server (a -> f) (a :+: ps) r m

data Method f ps r m = Method String ps f
