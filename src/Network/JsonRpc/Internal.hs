{-# LANGUAGE TypeOperators,
             OverloadedStrings #-}

module Network.JsonRpc.Internal where

import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:), (.:?))
import Data.Text (Text)
import Control.Applicative
import Control.Monad.Error

data ResultType r = ResultType

data Signature ps r = Signature String ps

resultType :: Signature ps r -> ResultType r
resultType _ = ResultType

data a :+: b = Text :+: b
infixr :+:

type RpcResult = ErrorT RpcError

data RpcError = RpcError { errorCode :: Int
                         , errorMessage :: String
                         , errorData :: Maybe A.Value } deriving Show

instance Error RpcError where
    noMsg = strMsg "unknown error"
    strMsg msg = RpcError (-31999) msg Nothing

instance A.ToJSON RpcError where
    toJSON e = A.object [ "code" .= errorCode e
                        , "message" .= errorMessage e
                        , "data" .= errorData e ]

instance A.FromJSON RpcError where
    parseJSON = A.withObject "JSON RPC error object" $
                \v -> RpcError <$>
                      v .: "code" <*>
                      v .: "message" <*>
                      v .:? "data"
