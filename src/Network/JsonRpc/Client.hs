{-# LANGUAGE OverloadedStrings,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators,
             FlexibleContexts,
             InstanceSigs #-}

module Network.JsonRpc.Client
              ( createClientFunction
              , createMonadFunction
              , runBatch) where

import Network.JsonRpc.Common
import Network.JsonRpc.Internal
import qualified Network.JsonRpc.Server as S
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as H
import Data.Text (Text ())
import Data.Scientific (Scientific ())
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.List (sortBy)
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Error

class Client f ps r | ps r -> f where
    createClientFunction' :: (Client f ps r) => String -> A.Object -> ps -> ResultType r -> f

instance A.FromJSON a => Client (Batch a) () a where
    createClientFunction' :: Client (Batch a) () a => String -> A.Object -> () -> ResultType a -> Batch a
    createClientFunction' name args _ _ = Batch [Request2 name args] toResult
                                               where toResult rs = fromResult $ A.fromJSON $ fromRight $ rsResult $ head rs
                                                     fromRight (Right x) = x

instance (Client f ps r, A.ToJSON a) => Client (a -> f) (a :+: ps) r where
    createClientFunction' :: Client (a -> f) (a :+: ps) r => String -> A.Object -> (a :+: ps) -> ResultType r -> a -> f
    createClientFunction' name args ((Param p) :+: ps) s a = createClientFunction' name (H.insert p (A.toJSON a) args) ps s

fromResult :: A.Result a -> a
fromResult (A.Success x) = x
fromResult (A.Error str) = error str

fromNum :: A.Value -> Scientific
fromNum (A.Number x) = x

createClientFunction :: Client f ps r => Signature ps r -> f
createClientFunction (Signature name ps r) = createClientFunction' name H.empty ps r

data Method f ps r m = Method String ps f

encodeBatch :: [Request] -> B.ByteString
encodeBatch [rq] = A.encode rq
encodeBatch rqs = A.encode rqs

decodeBatch :: A.Value -> [Response]
decodeBatch rs@(A.Array _) = fromResult $ A.fromJSON rs
decodeBatch r@(A.Object _) = [fromResult $ A.fromJSON r]

runBatch :: (Functor m, Monad m, A.FromJSON a)
         => (B.ByteString -> m B.ByteString) -> Batch a -> RpcResult m a
runBatch server (Batch rqs f) = let addId (Request2 method args) i = Request method (A.Number $ fromInteger i) args
                                    requests = zipWith addId rqs [1..]
                                    decode = fromResult . A.fromJSON . fromJust . A.decode
                                    process = lift . server . A.encode
                                    sendToServer = case requests of
                                                     [] -> return []
                                                     [rq] -> ((:[]) . decode) <$> process rq
                                                     rqs -> decode <$> process rqs
                                in do
                                  json <- sendToServer
                                  let sorted = sortBy (compare `on` fromNum . rsId) json
                                  return $ f sorted

data Batch a = Batch [Request2] ([Response] -> a)

instance Functor Batch where
    fmap f (Batch rqs g) = Batch rqs (f . g)

instance Applicative Batch where
    pure x = Batch [] (const x)
    (<*>) (Batch rqs1 f1) (Batch rqs2 f2) = let f3 rs = f1 rs1 $ f2 rs2
                                                    where (rs1, rs2) = splitAt (length rqs1) rs
                                            in Batch (rqs1 ++ rqs2) f3

createMonadFunction :: (Functor m, Monad m, A.FromJSON r, Client f ps r, Compose (Batch r) (RpcResult m r) f g)
                    => Signature ps r -> (B.ByteString -> m B.ByteString) -> g
createMonadFunction s server = compose (runBatch server) (createClientFunction s)

class Compose r1 r2 f1 f2 | f1 -> r1, r1 r2 f1 -> f2 where
    compose :: (r1 -> r2) -> f1 -> f2

instance A.FromJSON a => Compose (Batch a) b (Batch a) b where
    compose = ($)

instance Compose r1 r2 f1 f2 => Compose r1 r2 (a -> f1) (a -> f2) where
    compose f g = compose f . g

data Request2 = Request2 String A.Object

data Request = Request { rqMethod :: String
                       , rqId :: A.Value
                       , rqParams :: A.Object }

instance A.ToJSON Request where
    toJSON rq = A.object [ "jsonrpc" .= A.String "2.0"
                         , "method" .= rqMethod rq
                         , "id" .= rqId rq
                         , "params" .= rqParams rq ]

data Response = Response { rsResult :: Either RpcError A.Value
                         , rsId :: A.Value } deriving Show

instance A.FromJSON Response where
    parseJSON (A.Object v) = Response <$>
                             (Right <$> v .: "result") <*>
                             v .: "id"
    parseJSON _ = mzero
