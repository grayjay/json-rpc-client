{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators,
             FlexibleContexts #-}

-- | Functions for implementing the client side of JSON-RPC 2.0.
--   See <http://www.jsonrpc.org/specification>.
module Network.JsonRpc.Client ( -- * Types
                                Server
                              , RpcResult
                              -- * Signatures
                              , Signature (..)
                              , (:::) (..)
                              -- * Single Requests
                              , toFunction
                              , toFunction_
                              -- * Batch Requests
                              , Batch ()
                              , toBatchFunction
                              , toBatchFunction_
                              , voidBatch
                              , runBatch
                              -- * Errors
                              , RpcError (..)
                              , clientCode
                              -- * Type Classes
                              , ClientFunction
                              , ComposeMultiParam) where

import Network.JsonRpc.Server (RpcResult, RpcError (..), rpcError)
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import Data.Text (Text (), pack)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Lazy as H
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.List (sortBy)
import Control.Applicative (Applicative (..), Alternative (..), (<$>), (<*>), (<|>))
import Control.Monad.Error (ErrorT (..), throwError, lift, (<=<))

-- | Function used to send requests to the server.
--   'Nothing' represents no response, as when a JSON-RPC
--   server receives only notifications.
type Server m = ByteString -> m (Maybe ByteString)

type Result = Either RpcError

-- | Signature specifying the name,
--   parameter names and types ('ps'), and return type ('r') of a method.
data Signature ps r = Signature Text ps deriving Show

-- | A node in a linked list specifying parameter names and types.
--   It is right associative.
data p ::: ps = Text ::: ps deriving Show
infixr :::

-- | Creates a function for calling a JSON-RPC method as part of a batch request.
toBatchFunction :: ClientFunction ps r f =>
                   Signature ps r -- ^ Method signature.
                -> f              -- ^ Client-side function with a return type of @'Batch' r@.
toBatchFunction s@(Signature name params) = toBatch name H.empty params (resultType s)

-- | Creates a function for calling a JSON-RPC method as a notification and as part of a batch request.
toBatchFunction_ :: (ClientFunction ps r f, ComposeMultiParam (Batch r -> Batch ()) f g) =>
                    Signature ps r -- ^ Method signature.
                 -> g              -- ^ Client-side function with a return type of @'Batch' ()@.
toBatchFunction_ = composeWithBatch voidBatch

-- | Creates a function for calling a JSON-RPC method on the server.
toFunction :: (Monad m, Functor m, ClientFunction ps r f, ComposeMultiParam (Batch r -> RpcResult m r) f g) =>
              Server m       -- ^ Function for sending requests to the server.
           -> Signature ps r -- ^ Method signature.
           -> g              -- ^ Client-side function with a return type of @'RpcResult' m r@.
toFunction = composeWithBatch . runBatch

-- | Creates a function for calling a JSON-RPC method on the server as a notification.
toFunction_ :: (Monad m, Functor m, ClientFunction ps r f, ComposeMultiParam (Batch r -> RpcResult m ()) f g) =>
               Server m       -- ^ Function for sending requests to the server.
            -> Signature ps r -- ^ Method signature.
            -> g              -- ^ Client-side function with a return type of @'RpcResult' m ()@.
toFunction_ server = composeWithBatch $ runBatch server . voidBatch

composeWithBatch :: (ClientFunction ps r g, ComposeMultiParam f g h) => f -> Signature ps r -> h
composeWithBatch f = compose f . toBatchFunction

-- | Evaluates a batch.  The process depends on its size:
--   
-- 1. If the batch is empty, the server function is not called.
--   
-- 2. If the batch has exactly one request, it is sent as a request object.
--   
-- 3. If the batch has multiple requests, they are sent as an array of request objects.
runBatch :: (Monad m, Functor m) =>
            Server m      -- ^ Function for sending requests to the server.
         -> Batch r       -- ^ Batch to be evaluated.
         -> RpcResult m r -- ^ Result.
runBatch server batch = let addId rq2 i = Request (rq2Method rq2) idField (rq2Params rq2)
                                where idField = if rq2IsNotification rq2 then Nothing else Just $ A.Number $ fromInteger i
                            requests = zipWith addId (bRequests batch) [1..]
                            decode x = case A.eitherDecode x of
                                Left msg -> throwError $ rpcError clientCode $ pack $ "Client cannot parse JSON response: " ++ msg
                                Right r -> return r
                            process rq = lift $ server $ A.encode rq
                            sendToServer = case requests of
                                             [] -> return ([] :: [Response])
                                             [rq] -> maybe (return []) (((:[]) <$>) . decode) =<< process rq
                                             rqs -> maybe (return []) decode =<< process rqs
                            sort = sortBy (compare `on` rsId)
                        in do
                          json <- sendToServer
                          ErrorT $ return $ bToResult batch (sort json)

-- | Converts all requests in a batch to notifications.
voidBatch :: Batch r -> Batch ()
voidBatch batch = Batch { bNonNotifications = 0
                        , bRequests = map toNotification $ bRequests batch
                        , bToResult = const $ return () }
    where toNotification rq2 = rq2 { rq2IsNotification = True }

-- | A batch call.  Batch multiple requests by combining
--   values of this type using its 'Applicative' and 'Alternative'
--   instances before running them with 'runBatch'.
data Batch r = Batch { bNonNotifications :: Int
                     , bRequests :: [Request2]
                     , bToResult :: [Response] -> Result r }

instance Functor Batch where
    fmap f (Batch n rqs g) = Batch n rqs ((f <$>) . g)

instance Applicative Batch where
    pure x = Batch 0 [] (const $ return x)
    (<*>) = combine (<*>)

instance Alternative Batch where
    empty = Batch 0 [] (const $ throwError $ rpcError clientCode "")
    (<|>) = combine (<|>)

combine :: (Result a -> Result b -> Result c) -> Batch a -> Batch b -> Batch c
combine f (Batch n1 rqs1 g1) (Batch n2 rqs2 g2) = let g3 rs = g1 rs1 `f` g2 rs2
                                                          where (rs1, rs2) = splitAt n1 rs
                                                  in Batch (n1 + n2) (rqs1 ++ rqs2) g3

data ResultType r = ResultType

resultType :: Signature ps r -> ResultType r
resultType _ = ResultType

-- | Code used for all client-side errors.  It is -31999.
clientCode :: Int
clientCode = -31999

-- | Relationship between the parameters ('ps'), return type ('r'),
--   and client-side function ('f') of a JSON-RPC method.
class ClientFunction ps r f | ps r -> f, f -> ps r where
    toBatch :: Text -> A.Object -> ps -> ResultType r -> f

instance A.FromJSON r => ClientFunction () r (Batch r) where
    toBatch name args _ _ = Batch 1 [Request2 name False args] toResult
        where toResult = decode <=< rsResult . head
              decode rs = case A.fromJSON rs of
                           A.Success x -> Right x
                           A.Error msg -> Left $ rpcError clientCode $ pack $ "Client received wrong result type: " ++ msg

instance (ClientFunction ps r f, A.ToJSON a) => ClientFunction (a ::: ps) r (a -> f) where
    toBatch name args (p ::: ps) s a = toBatch name (H.insert p (A.toJSON a) args) ps s

-- | Relationship between a function ('g') taking any number of arguments and yielding a @'Batch' a@,
--   a function ('f') taking a @'Batch' a@, and the function ('h') formed by applying g to all of its
--   arguments and then applying f to the result.
class ComposeMultiParam f g h | f g -> h, g h -> f where
    compose :: f -> g -> h

instance ComposeMultiParam (Batch a -> b) (Batch a) b where
    compose = ($)

instance ComposeMultiParam f g h => ComposeMultiParam f (a -> g) (a -> h) where
    compose f g = compose f . g

data Request2 = Request2 { rq2Method :: Text
                         , rq2IsNotification :: Bool
                         , rq2Params :: A.Object }

data Request = Request { rqMethod :: Text
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
    parseJSON = A.withObject "JSON-RPC response object" $
                \v -> Response <$>
                      (Right <$> v .: "result" <|> Left <$> v .: "error")  <*>
                      v .: "id"
