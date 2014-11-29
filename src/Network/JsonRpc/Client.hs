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
              ( toFunction
              , toBatchFunction
              , toFunction_
              , toBatchFunction_
              , voidBatch
              , runBatch
              , Client
              , Batch ()
              , Compose) where

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
import Data.Maybe (catMaybes, fromJust)
import Data.List (sortBy)
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Error

class Client f ps r | ps r -> f where
    toBatch' :: String -> A.Object -> ps -> ResultType r -> f

instance A.FromJSON a => Client (Batch a) () a where
    toBatch' name args _ _ = Batch 1 [Request2 name False args] toResult
        where toResult rs = fromResult $ A.fromJSON $ fromRight $ rsResult $ head rs
              fromRight (Right x) = x

instance (Client f ps r, A.ToJSON a) => Client (a -> f) (a :+: ps) r where
    toBatch' name args ((Param p) :+: ps) s a = toBatch' name (H.insert p (A.toJSON a) args) ps s

fromResult :: A.Result a -> a
fromResult (A.Success x) = x
fromResult (A.Error str) = error str

fromNum :: A.Value -> Scientific
fromNum (A.Number x) = x

toBatchFunction :: Client f ps r => Signature ps r -> f
toBatchFunction s@(Signature name ps) = toBatch' name H.empty ps (resultType s)

data Method f ps r m = Method String ps f

encodeBatch :: [Request] -> B.ByteString
encodeBatch [rq] = A.encode rq
encodeBatch rqs = A.encode rqs

decodeBatch :: A.Value -> [Response]
decodeBatch rs@(A.Array _) = fromResult $ A.fromJSON rs
decodeBatch r@(A.Object _) = [fromResult $ A.fromJSON r]

runBatch :: (Functor m, Monad m)
         => (B.ByteString -> m B.ByteString) -> Batch a -> RpcResult m a
runBatch server batch = let addId rq2 i = Request (rq2Method rq2) idField (rq2Params rq2)
                                where idField = if rq2IsNotification rq2 then Nothing else Just $ A.Number $ fromInteger i
                            requests = zipWith addId (bRequests batch) [1..]
                            decode = fromResult . A.fromJSON . fromJust . A.decode
                            process = lift . server . A.encode
                            sendToServer = case requests of
                                             [] -> return []
                                             [rq] -> ((:[]) . decode) <$> process rq
                                             rqs -> decode <$> process rqs
                        in do
                          json <- sendToServer
                          let sorted = sortBy (compare `on` fromNum . rsId) json
                          return $ bToResult batch $ sorted

data Batch a = Batch { bNonNotifications :: Int
                     , bRequests :: [Request2]
                     , bToResult :: ([Response] -> a) }

instance Functor Batch where
    fmap f (Batch n rqs g) = Batch n rqs (f . g)

instance Applicative Batch where
    pure x = Batch 0 [] (const x)
    (<*>) (Batch n1 rqs1 f1) (Batch n2 rqs2 f2) = let f3 rs = f1 rs1 $ f2 rs2
                                                          where (rs1, rs2) = splitAt (n1) rs
                                                  in Batch (n1 + n2) (rqs1 ++ rqs2) f3

toFunction :: (Monad m, Functor m, Client f ps r, Compose (Batch r) (RpcResult m r) f g)
           => (B.ByteString -> m B.ByteString) -> Signature ps r -> g
toFunction server s = compose (runBatch server) (toBatchFunction s)

toBatchFunction_ :: (Client f ps r, Compose (Batch r) (Batch ()) f g) => Signature ps r -> g
toBatchFunction_ signature = compose voidBatch (toBatchFunction signature)

voidBatch :: Batch a -> Batch ()
voidBatch batch = Batch { bNonNotifications = 0
                        , bRequests = map toNotification $ bRequests batch
                        , bToResult = const () }
    where toNotification rq2 = rq2 { rq2IsNotification = True }

toFunction_ :: (Monad m, Functor m, Client f ps r, Compose (Batch r) (RpcResult m ()) f g)
            => (B.ByteString -> m B.ByteString) -> Signature ps r -> g
toFunction_ server signature = compose (runBatch server . voidBatch) (toBatchFunction signature)

class Compose r1 r2 f1 f2 | f1 -> r1, r1 r2 f1 -> f2 where
    compose :: (r1 -> r2) -> f1 -> f2

instance Compose (Batch a) b (Batch a) b where
    compose = ($)

instance Compose r1 r2 f1 f2 => Compose r1 r2 (a -> f1) (a -> f2) where
    compose f g = compose f . g

data Request2 = Request2 { rq2Method :: String
                         , rq2IsNotification :: Bool
                         , rq2Params :: A.Object }

data Request = Request { rqMethod :: String
                       , rqId :: Maybe A.Value
                       , rqParams :: A.Object }

instance A.ToJSON Request where
    toJSON rq = A.object $ catMaybes [ Just $ "jsonrpc" .= A.String "2.0"
                                     , Just $ "method" .= rqMethod rq
                                     , ("id" .=) <$> rqId rq
                                     , Just $ "params" .= rqParams rq]

data Response = Response { rsResult :: Either RpcError A.Value
                         , rsId :: A.Value } deriving Show

instance A.FromJSON Response where
    parseJSON (A.Object v) = Response <$>
                             (Right <$> v .: "result") <*>
                             v .: "id"
    parseJSON _ = mzero
