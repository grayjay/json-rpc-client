{-# LANGUAGE OverloadedStrings,
             TypeOperators #-}

module Main (main) where

import Network.JsonRpc.Client
import Network.JsonRpc.ServerAdapter
import Network.JsonRpc.Server (toMethods, rpcError, callWithBatchStrategy)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as B
import Data.Text (unpack)
import Data.List (isInfixOf)
import Control.Applicative (pure, empty, (<$>), (<$), (<*>), (<|>))
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.State (State, runState, evalState, modify, when)
import Test.HUnit hiding (State, Test)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Prelude hiding (subtract)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testCase "single" $ runServer (subtract 22 1) @?= (Right 21, 1)

        , testCase "batch length 1" $ myRunBatch (subtractB 1 2) @?= (Right (-1), 1)

        , let result = myRunBatch $ pure (,) <*> subtractB 7 3 <*> divideB 15 12
          in testCase "batch 2" $ result @?= (Right (4, 1.25), 2)

        , let result = myRunBatch $ (+) <$> subtractB 5 3 <*> ((*) <$> subtractB 11 2 <*> subtractB 15 10)
          in testCase "batch 3" $ result @?= (Right 47, 3)

        , let result = myRunBatch $ (*) <$> ((+) <$> subtractB 5 3 <*> subtractB 11 2) <*> subtractB 15 10
          in testCase "batch 4" $ result @?= (Right 55, 3)

        , testCase "notification" $ myRunBatch (subtractB_ 1 2) @?= (Right (), 1)

        , testCase "notification 2" $ runServer (subtract_ 1 2) @?= (Right (), 1)

        , let result = myRunBatch $ (,) <$> subtractB_ 5 4 <*> subtractB 20 16
          in testCase "batch with notification" $ result @?= (Right ((), 4), 2)

        , let result = myRunBatch $ (,) <$> subtractB 5 4 <*> subtractB_ 20 16
          in testCase "batch with notification 2" $ result @?= (Right (1, ()), 2)

        , let result = myRunBatch $ (:) <$> divideB 4 2 <*> ((:[]) <$> divideB 1 0)
          in testCase "batch with error" $ result @?= (Left (-32050), 1)

        , testCase "single error" $ runServer (divide 10 0) @?= (Left (-32050), 0)

        , let result = runServer $ runBatch badServer $ (||) <$> pure True <*> pure False
              badServer = error "unnecessarily sent to server"
          in testCase "empty request" $ result @?= (Right True, 0)

        , let result = myRunBatch $ divideB 1 0 <|> divideB 2 1
          in testCase "alternative 1" $ result @?= (Right 2, 1)

        , let result = myRunBatch $ divideB 2 1 <|> divideB 1 0
          in testCase "alternative 2" $ result @?= (Right 2, 1)

        , let result = myRunBatch $ divideB 2 0 <|> divideB 1 0
          in testCase "alternative 3" $ result @?= (Left (-32050), 0)

        , let result = myRunBatch $ divideB 2 1 <|> divideB 1 1
          in testCase "alternative 4" $ result @?= (Right 2, 2)

        , testCase "empty" $ myRunBatch (empty :: Batch String) @?= (Left (-31999), 0)

        , testCase "bad JSON result" $
                   assertErrorMsg "{" ["Client cannot parse JSON response"]

        , let response = A.encode $ A.object [ "result" .= A.Number 3
                                             , "jsonrpc" .= A.String "2.0" ]
          in testCase "missing response attribute" $ assertErrorMsg response
                 [ "Client cannot parse JSON response"
                 , "key \"id\" not present" ]

        , let response = A.encode $ A.object [ "id" .= A.Number 3
                                             , "result" .= True
                                             , "jsonrpc" .= A.String "2.0"]
          in testCase "wrong result type" $ assertErrorMsg response ["wrong result type"]

        , let response = A.encode [A.String "element"]
          in testCase "non-object response" $
             assertErrorMsg response ["expecting a JSON-RPC response"]

        , let response = A.encode $ A.object [ "id" .= A.Number 1
                                             , "error" .= A.Null
                                             , "jsonrpc" .= A.String "2.0" ]
          in testCase "wrong error type" $
             assertErrorMsg response ["expecting a JSON-RPC error"]
        ]

type Result r = RpcResult (State Int) r

subtractSig :: Signature (Int ::: Int ::: ()) Int
subtractSig = Signature "subtract" ("x" ::: "y" ::: ())

subtract = toFunction myServer subtractSig

subtract_ = toFunction_ myServer subtractSig

subtractB_ = toBatchFunction_ subtractSig

subtractB = toBatchFunction subtractSig

divideSig :: Signature (Double ::: Double ::: ()) Double
divideSig = Signature "divide" ("x" ::: "y" ::: ())

divide = toFunction myServer divideSig

divideB = toBatchFunction divideSig

runServer :: Result a -> (Either Int a, Int)
runServer server = runState (mapLeft errCode <$> runErrorT server) 0
    where mapLeft f (Left x) = Left $ f x
          mapLeft _ (Right x) = Right x

assertErrorMsgContains :: Result a -> String -> Assertion
assertErrorMsgContains server expected = case evalState (runErrorT server) 0 of
                                           Right _ -> assertFailure "Expected an error, but got a result."
                                           Left err -> assertBool msg $ expected `isInfixOf` errorMsg
                                               where errorMsg = unpack $ errMsg err
                                                     msg = "Wrong error message: " ++ errorMsg

assertErrorMsg :: B.ByteString -> [String] -> Assertion
assertErrorMsg response expected = (runServer result @?= (Left (-31999), 0)) >>
                                   mapM_ (assertErrorMsgContains result) expected
                 where result = toFunction badServer subtractSig 10 1
                       badServer = constServer response

myRunBatch :: A.FromJSON a => Batch a -> (Either Int a, Int)
myRunBatch = runServer . runBatch myServer

myServer :: B.ByteString -> State Int (Maybe B.ByteString)
myServer = callWithBatchStrategy (sequence . reverse) methods

constServer :: B.ByteString -> B.ByteString -> State Int (Maybe B.ByteString)
constServer = const . return . Just

methods = toMethods [subtractMethod, divideMethod]

subtractMethod = toServerMethod subtractSig f
    where f :: Int -> Int -> RpcResult (State Int) Int
          f x y = (x - y) <$ modify (+1)

divideMethod = toServerMethod divideSig f
    where f :: Double -> Double -> RpcResult (State Int) Double
          f x y = do
            when (y == 0) $ throwError $ rpcError (-32050) "divide by zero"
            x / y <$ modify (+1)
