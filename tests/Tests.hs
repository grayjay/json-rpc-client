{-# LANGUAGE OverloadedStrings,
             PackageImports,
             TypeOperators #-}

module Main (main) where

import Network.JsonRpc.Client
import Network.JsonRpc.Common
import qualified "json-rpc-server" Network.JsonRpc.Server as S
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Data.Maybe (fromJust)
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
        , let result = myRunBatch $ (+) <$> subtractB 5 3 <*> ((*) <$> subtractB 11 2 <*> subtractB 15 10)
          in testCase "batch 2" $ result @?= (Right 47, 3)
        , let result = myRunBatch $ (*) <$> ((+) <$> subtractB 5 3 <*> subtractB 11 2) <*> subtractB 15 10
          in testCase "batch 3" $ result @?= (Right 55, 3)
        , testCase "notification" $ myRunBatch (subtractB_ 1 2) @?= (Right (), 1)
        , testCase "notification 2" $ runServer (subtract_ 1 2) @?= (Right (), 1)
        , let result = myRunBatch $ (,) <$> subtractB_ 5 4 <*> subtractB 20 16
          in testCase "batch with notification" $ result @?= (Right ((), 4), 2)
        , let result = myRunBatch $ (,) <$> subtractB 5 4 <*> subtractB_ 20 16
          in testCase "batch with notification 2" $ result @?= (Right (1, ()), 2)
        ]

type Server = RpcResult (State Int)

subtractSig :: Signature (Double :+: Double :+: ()) Double
subtractSig = Signature "subtract" (Param "x" :+: Param "y" :+: ())

subtract = toFunction myServer subtractSig

subtract_ = toFunction_ myServer subtractSig

subtractB_ = toBatchFunction_ subtractSig

subtractB = toBatchFunction subtractSig

runServer :: Server a -> (Either Int a, Int)
runServer server = runState (mapLeft errorCode <$> runErrorT server) 0
    where mapLeft f (Left x) = Left $ f x
          mapLeft _ (Right x) = Right x

myRunBatch :: A.FromJSON a => Batch a -> (Either Int a, Int)
myRunBatch = runServer . runBatch myServer

myServer :: B.ByteString -> State Int B.ByteString
myServer = (fromJust <$>) . S.callWithBatchStrategy (sequence . reverse) methods

methods = S.toMethods [subtractMethod]

subtractMethod = S.toMethod "subtract" f params
    where params = S.Required "x" S.:+: S.Required "y" S.:+: ()
          f :: Double -> Double -> S.RpcResult (State Int) Double
          f x y = (x - y) <$ modify (+1)
