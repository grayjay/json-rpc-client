{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tests (tests) where

import Network.JsonRpc.Client
import Network.JsonRpc.ServerAdapter (toServerMethod)
import Network.JsonRpc.Server (rpcError, call, callWithBatchStrategy)

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Scientific (Scientific)
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State (State, runState, modify, when)
import Test.HUnit hiding (State, Test)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Prelude hiding (subtract)
import Control.Applicative (empty, (<|>))

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>), (<$), (<*>))
#endif

tests :: [Test]
tests = [ testCase "single request" $
              runResult (subtract 22 1) @?= (Right 21, 1)

        , testCase "batch with one request" $
              myRunBatch (subtractB 1 2) @?= (Right (-1), 1)

        , testCase "batch with multiple requests" $
              let result = myRunBatch $ pure (,) <*> subtractB 7 3 <*> divideB 15 12
              in result @?= (Right (4, 5%4), 2)

        , testCase "requests combined right to left" $
              let result = myRunBatch $
                           (+) <$> subtractB 5 3 <*> ((*) <$> subtractB 11 2 <*> subtractB 15 10)
              in result @?= (Right 47, 3)

        , testCase "requests combined left to right" $
              let result = myRunBatch $
                           (*) <$> ((+) <$> subtractB 5 3 <*> subtractB 11 2) <*> subtractB 15 10
              in result @?= (Right 55, 3)

        , testCase "single notification" $
                   runResult (subtract_ 1 2) @?= (Right (), 1)

        , testCase "batch with one notification" $
                   myRunBatch (subtractB_ 1 2) @?= (Right (), 1)

        , testCase "notification at start of batch" $
              let result = myRunBatch $
                           (,) <$> subtractB_ 5 4 <*> subtractB 20 16
              in result @?= (Right ((), 4), 2)

        , testCase "notification at end of batch" $
              let result = myRunBatch $
                           (,) <$> subtractB 5 4 <*> subtractB_ 20 16
              in result @?= (Right (1, ()), 2)

        , testCase "single request with error" $
              runResult (divide 10 0) @?= (Left divByZeroCode, 0)

        , testCase "error at start of batch" $
              let result = myRunBatch $
                           (:) <$> divideB 4 0 <*> ((:[]) <$> divideB 1 1)
              in result @?= (Left divByZeroCode, 1)

        , testCase "error at end of batch" $
              let result = myRunBatch $
                           (+) <$> divideB 4 2 <*> divideB 1 0
              in result @?= (Left divByZeroCode, 1)

        , testCase "batch with multiple errors" $
              let result = myRunBatch $
                           (,) <$> divideB 1 0 <*> missingMethodB
              in result @?= (Left divByZeroCode, 0)

        , testCase "batch with no requests is not sent to server" $
              let result = runResult $
                           runBatch badServer $ (||) <$> pure True <*> pure False
                  badServer = constServer $ error "server was used"
              in result @?= (Right True, 0)

        , testCase "batch alternative with failure first" $
              let result = myRunBatch $ divideB 1 0 <|> divideB 2 1
              in result @?= (Right 2, 1)

        , testCase "batch alternative with failure last" $
              let result = myRunBatch $ divideB 2 1 <|> divideB 1 0
              in result @?= (Right 2, 1)

        , testCase "batch alternative with all failures" $
              let result = myRunBatch $ divideB 2 0 <|> missingMethodB
              in result @?= (Left (-32601), 0)

        , testCase "batch alternative with no failures" $
              let result = myRunBatch $ divideB 2 1 <|> divideB 1 1
              in result @?= (Right 2, 2)

        , testCase "empty batch" $
              myRunBatch (empty :: Batch String) @?= (Left (-31999), 0)

        , testCase "invalid JSON response" $
              let result = subtractWithConstServer $ A.Number 3
              in runResult result @?= (Left clientCode, 0)

        , testCase "missing ID attribute in response" $
              let result = subtractWithConstServer $
                               A.object [ "result" .= A.Number 3
                                        , "jsonrpc" .= A.String "2.0" ]
              in runResult result @?= (Left clientCode, 0)

        , testCase "non-object response to single request" $
              let result = subtractWithConstServer $ A.toJSON [A.String "element"]
              in runResult result @?= (Left clientCode, 0)

        , testCase "wrong result type in response" $
              let result = subtractWithConstServer $
                               A.object [ "id" .= A.Number 3
                                        , "result" .= True
                                        , "jsonrpc" .= A.String "2.0" ]
              in runResult result @?= (Left clientCode, 0)

        , testCase "detect modified single request ID" $
              let result = toFunction (idModifyingServer (+1)) subtractSig 10 1
              in runResult result @?= (Left clientCode, 1)

        , testCase "detect modified batch response IDs" $
              let result = runBatch (idModifyingServer (+1)) $
                           (+) <$> subtractB 10 1 <*> subtractB 2 1
              in runResult result @?= (Left clientCode, 2)

        , testCase "detect duplicate batch response IDs" $
              let result = runBatch (idModifyingServer (const 0)) $
                           (,) <$> divideB 3 1 <*> divideB 3 1
              in runResult result @?= (Left clientCode, 2)

        , testCase "handle reversed batch responses" $
              let result = runBatch reversingServer $
                           (,) <$> subtractB 2 1 <*> subtractB 4 2
                  reversingServer = callWithBatchStrategy (sequence . reverse) methods
              in runResult result @?= (Right (1, 2), 2)
        ]

-- | Monad used by server to keep a count of requests.
type RequestCount = State Int

type Result r = RpcResult RequestCount r

subtractSig :: Signature (Int ::: Int ::: ()) Int
subtractSig = Signature "subtract" ("x" ::: "y" ::: ())

divideSig :: Signature (Rational ::: Rational ::: ()) Rational
divideSig = Signature "divide" ("x" ::: "y" ::: ())

missingMethodSig :: Signature () Rational
missingMethodSig = Signature "f" ()

subtract = toFunction myServer subtractSig

subtract_ = toFunction_ myServer subtractSig

subtractB = toBatchFunction subtractSig

subtractB_ = toBatchFunction_ subtractSig

divide = toFunction myServer divideSig

divideB = toBatchFunction divideSig

missingMethodB = toBatchFunction missingMethodSig

-- | Returns the error code or result, and the new server state.
runResult :: Result a -> (Either Int a, Int)
runResult result = runState (mapLeft errCode <$> runExceptT result) 0
    where mapLeft f (Left x) = Left $ f x
          mapLeft _ (Right x) = Right x

myRunBatch :: Batch a -> (Either Int a, Int)
myRunBatch = runResult . runBatch myServer

myServer :: B.ByteString -> RequestCount (Maybe B.ByteString)
myServer = call methods

subtractWithConstServer :: A.Value -> RpcResult RequestCount Int
subtractWithConstServer response = toFunction server subtractSig 1 2
    where server = constServer $ A.encode response

idModifyingServer :: (Scientific -> Scientific) -> Connection RequestCount
idModifyingServer f = responseModifyingServer modifyIds
    where modifyIds (A.Array rs) = A.Array $ V.map modifyIds rs
          modifyIds (A.Object r) = A.Object $ M.adjust modifyId "id" r
              where modifyId (A.Number i) = A.Number $ f i
                    modifyId x = x
          modifyIds x = x

responseModifyingServer :: (A.Value -> A.Value) -> Connection RequestCount
responseModifyingServer f rq = modifyResponse <$> myServer rq
    where modifyResponse rsp = A.encode . f . fromJust . A.decode <$> rsp

constServer :: B.ByteString -> Connection RequestCount
constServer = const . return . Just

methods = [subtractMethod, divideMethod]

subtractMethod = toServerMethod subtractSig f
    where f :: Int -> Int -> RpcResult RequestCount Int
          f x y = (x - y) <$ modify (+1)

divideMethod = toServerMethod divideSig f
    where f :: Rational -> Rational -> RpcResult RequestCount Rational
          f x y = do
            when (y == 0) $ throwError $ rpcError divByZeroCode "divide by zero"
            x / y <$ modify (+1)

divByZeroCode = -32050
