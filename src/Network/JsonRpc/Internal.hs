{-# LANGUAGE TypeOperators #-}

module Network.JsonRpc.Internal where

import Data.Text
import Control.Monad.Error

data ResultType r = ResultType

data Signature ps r = Signature String ps (ResultType r)

signature :: String -> ps -> Signature ps r
signature method params = Signature method params ResultType

data Param a = Param Text

data a :+: b = (Param a) :+: b
infixr :+:

type RpcResult = ErrorT RpcError

data RpcError = RpcError Int String deriving Show

instance Error RpcError
