{-# LANGUAGE TypeOperators #-}

module Network.JsonRpc.Internal where

import qualified Data.Aeson as A
import Data.Text
import Control.Monad.Error

data ResultType r = ResultType

data Signature ps r = Signature String ps

resultType :: Signature ps r -> ResultType r
resultType _ = ResultType

data Param a = Param Text

data a :+: b = (Param a) :+: b
infixr :+:

type RpcResult = ErrorT RpcError

data RpcError = RpcError { errorCode :: Int
                         , errorMessage :: String
                         , errorData :: A.Value } deriving Show

instance Error RpcError
