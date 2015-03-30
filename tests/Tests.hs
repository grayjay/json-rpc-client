{-# LANGUAGE CPP,
             OverloadedStrings,
             TypeOperators #-}

#if MIN_VERSION_mtl(2,2,1)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Tests (tests) where

import Network.JsonRpc.Client
import Network.JsonRpc.ServerAdapter
import Network.JsonRpc.Server (toMethods, rpcError, callWithBatchStrategy)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as B
import Data.Text (unpack)
import Data.List (isInfixOf)
import Control.Monad.Error (runErrorT, throwError)
import Control.Monad.State (State, runState, evalState, modify, when)
import Test.HUnit hiding (State, Test)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Prelude hiding (subtract)
import Control.Applicative (empty, (<|>))

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>), (<$), (<*>))
#endif

tests :: [Test]
tests = [ testCase "single request" $ runServer (subtract 22 1) @?= (Right 21, 1)

        , testCase "batch with one request" $ myRunBatch (subtractB 1 2) @?= (Right (-1), 1)

        , let result = myRunBatch $ pure (,) <*> subtractB 7 3 <*> divideB 15 12
          in testCase "batch 1" $ result @?= (Right (4, 1.25), 2)

        , let result = myRunBatch $ (+) <$> subtractB 5 3 <*> ((*) <$> subtractB 11 2 <*> subtractB 15 10)
          in testCase "batch 2" $ result @?= (Right 47, 3)

        , let result = myRunBatch $ (*) <$> ((+) <$> subtractB 5 3 <*> subtractB 11 2) <*> subtractB 15 10
          in testCase "batch 3" $ result @?= (Right 55, 3)

        , testCase "single notification" $
                   runServer (subtract_ 1 2) @?= (Right (), 1)

        , testCase "batch with one notification" $
                   myRunBatch (subtractB_ 1 2) @?= (Right (), 1)

        , let result = myRunBatch $ (,) <$> subtractB_ 5 4 <*> subtractB 20 16
          in testCase "notification at start of batch" $ result @?= (Right ((), 4), 2)

        , let result = myRunBatch $ (,) <$> subtractB 5 4 <*> subtractB_ 20 16
          in testCase "notification at end of batch" $ result @?= (Right (1, ()), 2)

        , let result = myRunBatch $ (:) <$> divideB 4 2 <*> ((:[]) <$> divideB 1 0)
          in testCase "batch with error" $ result @?= (Left (-32050), 1)

        , testCase "single request with error" $ runServer (divide 10 0) @?= (Left (-32050), 0)

        , let result = runServer $ runBatch badServer $ (||) <$> pure True <*> pure False
              badServer = error "unnecessarily sent to server"
          in testCase "batch with no requests" $ result @?= (Right True, 0)

        , let result = myRunBatch $ divideB 1 0 <|> divideB 2 1
          in testCase "batch alternative with failure first" $ result @?= (Right 2, 1)

        , let result = myRunBatch $ divideB 2 1 <|> divideB 1 0
          in testCase "batch alternative with failure last" $ result @?= (Right 2, 1)

        , let result = myRunBatch $ divideB 2 0 <|> divideB 1 0
          in testCase "batch alternative with all failures" $ result @?= (Left (-32050), 0)

        , let result = myRunBatch $ divideB 2 1 <|> divideB 1 1
          in testCase "batch alternative with no failures" $ result @?= (Right 2, 2)

        , testCase "empty batch" $ myRunBatch (empty :: Batch String) @?= (Left (-31999), 0)

        , testCase "bad JSON response" $
                   assertErrorMsg (A.encode $ A.Number 3) ["Client cannot parse JSON response"]

        , let response = A.encode $ A.object [ "result" .= A.Number 3
                                             , "jsonrpc" .= A.String "2.0" ]
          in testCase "missing response id attribute" $ assertErrorMsg response
                 [ "Client cannot parse JSON response"
                 , "key \"id\" not present" ]

        , let response = A.encode [A.String "element"]
          in testCase "non-object response" $
             assertErrorMsg response ["expecting a JSON-RPC response"]

        , let response = A.encode $ A.object [ "id" .= A.Number 3
                                             , "result" .= True
                                             , "jsonrpc" .= A.String "2.0"]
          in testCase "wrong response result type" $
             assertErrorMsg response ["wrong result type"]
        ]

type Result r = RpcResult (State Int) r

subtractSig :: Signature (Int ::: Int ::: ()) Int
subtractSig = Signature "subtract" ("x" ::: "y" ::: ())

divideSig :: Signature (Double ::: Double ::: ()) Double
divideSig = Signature "divide" ("x" ::: "y" ::: ())

subtract = toFunction myServer subtractSig

subtract_ = toFunction_ myServer subtractSig

subtractB = toBatchFunction subtractSig

subtractB_ = toBatchFunction_ subtractSig

divide = toFunction myServer divideSig

divideB = toBatchFunction divideSig

runServer :: Result a -> (Either Int a, Int)
runServer server = runState (mapLeft errCode <$> runErrorT server) 0
    where mapLeft f (Left x) = Left $ f x
          mapLeft _ (Right x) = Right x

myRunBatch :: A.FromJSON a => Batch a -> (Either Int a, Int)
myRunBatch = runServer . runBatch myServer

myServer :: B.ByteString -> State Int (Maybe B.ByteString)
myServer = callWithBatchStrategy (sequence . reverse) methods

assertErrorMsg :: B.ByteString -> [String] -> Assertion
assertErrorMsg response expected = (runServer result @?= (Left (-31999), 0)) >>
                                   mapM_ (assertErrorMsgContains result) expected
                 where result = toFunction badServer subtractSig 10 1
                       badServer = constServer response

constServer :: B.ByteString -> B.ByteString -> State Int (Maybe B.ByteString)
constServer = const . return . Just

assertErrorMsgContains :: Result a -> String -> Assertion
assertErrorMsgContains server expected = case evalState (runErrorT server) 0 of
                                           Right _ -> assertFailure "Expected an error, but got a result."
                                           Left err -> assertBool msg $ expected `isInfixOf` errorMsg
                                               where errorMsg = unpack $ errMsg err
                                                     msg = "Wrong error message: " ++ errorMsg

methods = toMethods [subtractMethod, divideMethod]

subtractMethod = toServerMethod subtractSig f
    where f :: Int -> Int -> RpcResult (State Int) Int
          f x y = (x - y) <$ modify (+1)

divideMethod = toServerMethod divideSig f
    where f :: Double -> Double -> RpcResult (State Int) Double
          f x y = do
            when (y == 0) $ throwError $ rpcError (-32050) "divide by zero"
            x / y <$ modify (+1)
