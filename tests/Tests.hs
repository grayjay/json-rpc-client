{-# LANGUAGE OverloadedStrings,
             PackageImports,
             TypeOperators #-}

module Main (main) where

import Network.JsonRpc.Client
import Network.JsonRpc.Common
import qualified "json-rpc-server" Network.JsonRpc.Server as S
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as B
import Text.Regex.Posix ((=~))
import Control.Applicative
import Data.Maybe (fromMaybe)
import Control.Monad.Error
import Control.Monad.State
import Test.HUnit hiding (State, Test)
import Test.Framework (defaultMain, Test)
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

        , let result = toFunction (constServer "{") subtractSig 2 1
          in testCase "bad JSON response" $
             (runServer result @?= (Left (-31999), 0)) >>
             assertErrorMsg result "Client .* JSON"

        , let result = toFunction badServer subtractSig 2 1
              badServer = constServer $ A.encode $ A.object ["result" .= True]
          in testCase "bad JSON response" $
             (runServer result @?= (Left (-31999), 0)) >>
             assertErrorMsg result "Client cannot parse JSON" >>
             assertErrorMsg result "key .* not present"
        ]

type Server = RpcResult (State Int)

subtractSig :: Signature (Int :+: Int :+: ()) Int
subtractSig = Signature "subtract" ("x" :+: "y" :+: ())

subtract = toFunction myServer subtractSig

subtract_ = toFunction_ myServer subtractSig

subtractB_ = toBatchFunction_ subtractSig

subtractB = toBatchFunction subtractSig

divideSig :: Signature (Double :+: Double :+: ()) Double
divideSig = Signature "divide" ("x" :+: "y" :+: ())

divide = toFunction myServer divideSig

divideB = toBatchFunction divideSig

runServer :: Server a -> (Either Int a, Int)
runServer server = runState (mapLeft errorCode <$> runErrorT server) 0
    where mapLeft f (Left x) = Left $ f x
          mapLeft _ (Right x) = Right x

assertErrorMsg :: Server a -> String -> Assertion
assertErrorMsg server expected = case evalState (runErrorT server) 0 of
                                   Right _ -> assertFailure "Expected an error, but got a result."
                                   Left err -> assertBool msg $ errMsg =~ expected
                                       where errMsg = errorMessage err
                                             msg = "Wrong error message: " ++ errMsg

myRunBatch :: A.FromJSON a => Batch a -> (Either Int a, Int)
myRunBatch = runServer . runBatch myServer

myServer :: B.ByteString -> State Int B.ByteString
myServer = (fromMaybe "" <$>) . S.callWithBatchStrategy (sequence . reverse) methods

constServer :: B.ByteString -> B.ByteString -> State Int B.ByteString
constServer = const . return

methods = S.toMethods [subtractMethod, divideMethod]

subtractMethod = S.toMethod "subtract" f params
    where params = S.Required "x" S.:+: S.Required "y" S.:+: ()
          f :: Int -> Int -> S.RpcResult (State Int) Int
          f x y = (x - y) <$ modify (+1)

divideMethod = S.toMethod "divide" f params
    where params = S.Required "x" S.:+: S.Required "y" S.:+: ()
          f :: Double -> Double -> S.RpcResult (State Int) Double
          f x y = do
            when (y == 0) $ throwError $ S.rpcError (-32050) "divide by zero"
            x / y <$ modify (+1)
