{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators,
             FlexibleContexts #-}

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
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as H
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.List (sortBy)
import Control.Applicative
import Control.Monad.Error

class Client f ps r | ps r -> f where
    toBatch :: String -> A.Object -> ps -> ResultType r -> f

instance A.FromJSON a => Client (Batch a) () a where
    toBatch name args _ _ = Batch 1 [Request2 name False args] toResult
        where toResult = decode <=< rsResult . head
              decode rs = case A.fromJSON rs of
                           A.Success x -> Right x
                           A.Error msg -> Left $ strMsg $ "Client received wrong result type: " ++ msg

instance (Client f ps r, A.ToJSON a) => Client (a -> f) (a :+: ps) r where
    toBatch name args (p :+: ps) s a = toBatch name (H.insert p (A.toJSON a) args) ps s

toBatchFunction :: Client f ps r => Signature ps r -> f
toBatchFunction s@(Signature name ps) = toBatch name H.empty ps (resultType s)

runBatch :: (Functor m, Monad m)
         => (B.ByteString -> m B.ByteString) -> Batch a -> RpcResult m a
runBatch server batch = let addId rq2 i = Request (rq2Method rq2) idField (rq2Params rq2)
                                where idField = if rq2IsNotification rq2 then Nothing else Just $ A.Number $ fromInteger i
                            requests = zipWith addId (bRequests batch) [1..]
                            decode x = case A.eitherDecode x of
                                Left msg -> throwError $ strMsg $ "Client cannot parse JSON response: " ++ msg
                                Right r -> return r
                            process rq = lift $ server $ A.encode rq
                            sendToServer = case requests of
                                             [] -> return []
                                             [rq] -> applyNonNull (((:[]) <$>) . decode) =<< process rq
                                             rqs -> applyNonNull decode =<< process rqs
                            applyNonNull f rsp = if B.null rsp then return [] else f rsp
                            sort = sortBy (compare `on` rsId)
                        in do
                          json <- sendToServer
                          ErrorT $ return $ bToResult batch (sort json)

type Result = Either RpcError

data Batch a = Batch { bNonNotifications :: Int
                     , bRequests :: [Request2]
                     , bToResult :: [Response] -> Result a }

instance Functor Batch where
    fmap f (Batch n rqs g) = Batch n rqs ((f <$>) . g)

instance Applicative Batch where
    pure x = Batch 0 [] (const $ return x)
    (<*>) = combine (<*>)

instance Alternative Batch where
    empty = Batch 0 [] (const $ throwError noMsg)
    (<|>) = combine (<|>)

combine :: (Result a -> Result b -> Result c) -> Batch a -> Batch b -> Batch c
combine f (Batch n1 rqs1 g1) (Batch n2 rqs2 g2) = let g3 rs = g1 rs1 `f` g2 rs2
                                                          where (rs1, rs2) = splitAt n1 rs
                                                  in Batch (n1 + n2) (rqs1 ++ rqs2) g3

toFunction :: (Monad m, Functor m, Client f ps r, Compose (Batch r) (RpcResult m r) f g)
           => (B.ByteString -> m B.ByteString) -> Signature ps r -> g
toFunction server s = compose (runBatch server) (toBatchFunction s)

toBatchFunction_ :: (Client f ps r, Compose (Batch r) (Batch ()) f g) => Signature ps r -> g
toBatchFunction_ signature = compose voidBatch (toBatchFunction signature)

voidBatch :: Batch a -> Batch ()
voidBatch batch = Batch { bNonNotifications = 0
                        , bRequests = map toNotification $ bRequests batch
                        , bToResult = const $ return () }
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
                         , rsId :: Int } deriving Show

instance A.FromJSON Response where
    parseJSON = A.withObject "JSON RPC response object" $
                \v -> Response <$>
                      (Right <$> v .: "result" <|> Left <$> v .: "error")  <*>
                      v .: "id"
