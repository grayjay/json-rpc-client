{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for implementing the client side of JSON-RPC 2.0.
--   See <http://www.jsonrpc.org/specification>.


module Network.JsonRpc.Client ( -- * Summary
                                -- $summary

                                -- * Demo
                                -- $demo

                                -- * Types
                                  Connection
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
                                -- $errors
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
import qualified Data.HashMap.Strict as H
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import Control.Arrow ((&&&))
import Control.Monad (liftM)
import Control.Monad.Except (ExceptT (..), throwError, lift, (<=<))
import Control.Applicative (Alternative (..), (<|>))

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>), (<*>))
#endif

-- $summary
-- * Create one 'Signature' for each server-side method to be called.
--   'Signature's can be shared between client and server, using
--   'Network.JsonRpc.ServerAdapter.toServerMethod'.
-- * Create a function of type @Monad m => 'Connection' m@ for communicating
--   with the server.
-- * Create client-side functions by calling 'toFunction', 'toFunction_',
--   'toBatchFunction', or 'toBatchFunction_' on the 'Signature's.
-- * Client-side functions created with 'toBatchFunction' or 'toBatchFunction_'
--   return values of type @'Batch' a@.  Combine them using 'Batch' 's
--   'Applicative' and 'Alternative' instances, before calling 'runBatch'
--   on the result.

-- $demo
-- The <../src/demo demo folder> contains a client and server that communicate
-- using a shared set of 'Signature's.  The client runs the server as a
-- subprocess, sending requests to stdin and receiving responses from stdout.
-- Compile both programs with the @demo@ flag.  Then run the client by passing
-- it a command to run the server (e.g., @demo-client demo-server@).

-- $errors
-- 'RpcError' is used for all server-side errors, as described in the
-- specification.  Additionally, the error code @-31999@ is used for any
-- errors that occur while parsing the server's response.

-- | Function used to send requests to the server.
--   'Nothing' represents no response, as when a JSON-RPC
--   server receives only notifications.
type Connection m = ByteString -> m (Maybe ByteString)

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
toBatchFunction s@(Signature name params) = _toBatch name params (resultType s) H.empty

-- | Creates a function for calling a JSON-RPC method as a notification and as part of a batch request.
toBatchFunction_ :: (ClientFunction ps r f, ComposeMultiParam (Batch r -> Batch ()) f g) =>
                    Signature ps r -- ^ Method signature.
                 -> g              -- ^ Client-side function with a return type of @'Batch' ()@.
toBatchFunction_ = composeWithBatch voidBatch

-- | Creates a function for calling a JSON-RPC method on the server.
toFunction :: (Monad m, ClientFunction ps r f, ComposeMultiParam (Batch r -> RpcResult m r) f g) =>
              Connection m   -- ^ Function for sending requests to the server.
           -> Signature ps r -- ^ Method signature.
           -> g              -- ^ Client-side function with a return type of @'RpcResult' m r@.
toFunction = composeWithBatch . runBatch

-- | Creates a function for calling a JSON-RPC method on the server as a notification.
toFunction_ :: (Monad m, ClientFunction ps r f, ComposeMultiParam (Batch r -> RpcResult m ()) f g) =>
               Connection m   -- ^ Function for sending requests to the server.
            -> Signature ps r -- ^ Method signature.
            -> g              -- ^ Client-side function with a return type of @'RpcResult' m ()@.
toFunction_ server = composeWithBatch $ runBatch server . voidBatch

composeWithBatch :: (ClientFunction ps r g, ComposeMultiParam f g h) => f -> Signature ps r -> h
composeWithBatch f = _compose f . toBatchFunction

-- | Evaluates a batch.  The process depends on its size:
--   
-- 1. If the batch is empty, the server function is not called.
--   
-- 2. If the batch has exactly one request, it is sent as a request object.
--   
-- 3. If the batch has multiple requests, they are sent as an array of request objects.
runBatch :: Monad m =>
            Connection m  -- ^ Function for sending requests to the server.
         -> Batch r       -- ^ Batch to be evaluated.
         -> RpcResult m r -- ^ Result.
runBatch server batch = liftResult . bToResult batch =<<
                        validate . sort =<<
                        processRqs server idRequests
    where requests = bRequests batch
          idRequests = V.zipWith assignId requests ids
              where ids = V.postscanl' incId 0 requests
                    incId i rq = if rqIsNotification rq then i else i + 1
          sort = V.modify $ VA.sortBy $ comparing rsId
          liftResult = ExceptT . return
          validate rsps = let (results, ids) = V.unzip $ V.map (rsResult &&& rsId) rsps
                          in if ids /= V.enumFromN 1 (bNonNotifications batch)
                             then throwError $ clientError $
                                      "Invalid response IDs: " ++ show ids
                             else return results

assignId :: Request -> Int -> IdRequest
assignId rq i = IdRequest { idRqMethod = rqMethod rq
                          , idRqId = if rqIsNotification rq then Nothing else Just i
                          , idRqParams = rqParams rq }

processRqs :: Monad m =>
              Connection m -> V.Vector IdRequest -> RpcResult m (V.Vector Response)
processRqs server requests | V.null requests = return V.empty
                           | V.length requests == 1 = process V.singleton $ V.head requests
                           | otherwise = process id requests
    where decode rsp = case A.eitherDecode rsp of
                         Right r -> return r
                         Left msg -> throwError $ clientError $
                                     "Client cannot parse JSON response: " ++ msg
          process f rqs = maybe (return V.empty) (liftM f . decode) =<<
                          (lift . server . A.encode) rqs

-- | Converts all requests in a batch to notifications.
voidBatch :: Batch r -> Batch ()
voidBatch batch = Batch { bNonNotifications = 0
                        , bRequests = V.map toNotification $ bRequests batch
                        , bToResult = const $ return () }
    where toNotification rq = rq { rqIsNotification = True }

-- | A batch call.  Batch multiple requests by combining
--   values of this type using its 'Applicative' and 'Alternative'
--   instances before running them with 'runBatch'.
data Batch r = Batch { bNonNotifications :: Int
                     , bRequests :: V.Vector Request
                     , bToResult :: V.Vector (Result A.Value) -> Result r }

instance Functor Batch where
    fmap f batch = batch { bToResult = fmap f . bToResult batch }

instance Applicative Batch where
    pure x = empty { bToResult = const $ return x }
    (<*>) = combine (<*>)

instance Alternative Batch where
    empty = Batch { bNonNotifications = 0
                  , bRequests = V.empty
                  , bToResult = const $ throwError $ clientError "empty" }
    (<|>) = combine (<||>)
        where Right x <||> _ = Right x
              _ <||> x = x

combine :: (Result a -> Result b -> Result c) -> Batch a -> Batch b -> Batch c
combine f (Batch n1 rqs1 g1) (Batch n2 rqs2 g2) =
    Batch { bNonNotifications = n1 + n2
          , bRequests = rqs1 V.++ rqs2
          , bToResult = \rs -> let (rs1, rs2) = V.splitAt n1 rs
                               in g1 rs1 `f` g2 rs2 }

data ResultType r = ResultType

resultType :: Signature ps r -> ResultType r
resultType _ = ResultType

clientError :: String -> RpcError
clientError msg = rpcError clientCode $ pack msg

-- | Code used for all client-side errors.  It is -31999.
clientCode :: Int
clientCode = -31999

-- | Relationship between the parameters ('ps'), return type ('r'),
--   and client-side batch function ('f') of a JSON-RPC method.
class ClientFunction ps r f | ps r -> f, f -> ps r where
    _toBatch :: Text -> ps -> ResultType r -> A.Object -> f

instance A.FromJSON r => ClientFunction () r (Batch r) where
    _toBatch name _ _ priorArgs = Batch { bNonNotifications = 1
                                       , bRequests = V.singleton $
                                                     Request name False priorArgs
                                       , bToResult = decode <=< V.head }
        where decode result = case A.fromJSON result of
                                A.Success r -> Right r
                                A.Error msg -> throwError . clientError $
                                               "Client received wrong result type: " ++ msg

instance (ClientFunction ps r f, A.ToJSON a) => ClientFunction (a ::: ps) r (a -> f) where
    _toBatch name (p ::: ps) rt priorArgs a = let newArgs = H.insert p (A.toJSON a) priorArgs
                                              in _toBatch name ps rt newArgs

-- | Relationship between a function ('g') taking any number of arguments and yielding a @'Batch' a@,
--   a function ('f') taking a @'Batch' a@, and the function ('h') that applies g to all of its
--   arguments and then applies f to the result.
class ComposeMultiParam f g h | f g -> h, g h -> f where
    _compose :: f -> g -> h

instance ComposeMultiParam (Batch a -> b) (Batch a) b where
    _compose = ($)

instance ComposeMultiParam f g h => ComposeMultiParam f (a -> g) (a -> h) where
    _compose f g = _compose f . g

data Request = Request { rqMethod :: Text
                       , rqIsNotification :: Bool
                       , rqParams :: A.Object }

data IdRequest = IdRequest { idRqMethod :: Text
                           , idRqId :: Maybe Int
                           , idRqParams :: A.Object }

instance A.ToJSON IdRequest where
    toJSON rq = A.object $ catMaybes [ Just $ "jsonrpc" .= A.String "2.0"
                                     , Just $ "method" .= idRqMethod rq
                                     , ("id" .=) <$> idRqId rq
                                     , let params = idRqParams rq
                                       in if H.null params
                                          then Nothing
                                          else Just $ "params" .= params ]

data Response = Response { rsResult :: Result A.Value
                         , rsId :: Int }

instance A.FromJSON Response where
    parseJSON = A.withObject "JSON-RPC response object" $
                \v -> Response <$>
                      (Right <$> v .: "result" <|> Left <$> v .: "error")  <*>
                      v .: "id"
