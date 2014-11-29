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
import Data.Functor
import Control.Monad.Error
import Control.Monad.State
import Test.HUnit hiding (State, Test)
import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testCase "single" $ getResult (singleSubtract 22 1) @?= (21, 1)
        , testCase "batch length 1" $ getResult (myRunBatch $ s 1 2) @?= ((-1), 1)
        , let result = getResult $ myRunBatch $ (+) <$> s 5 3 <*> ((*) <$> s 11 2 <*> s 15 10)
          in testCase "batch 2" $ result @?= (47, 3)
        , let result = getResult $ myRunBatch $ (*) <$> ((+) <$> s 5 3 <*> s 11 2) <*> s 15 10
          in testCase "batch 3" $ result @?= (55, 3)
        , testCase "notification" $ getResult (myRunBatch $ subtractNotification 1 2) @?= ((), 1)
        , let result = getResult $ myRunBatch $ (,) <$> subtractNotification 5 4 <*> s 20 16
          in testCase "batch with notification" $ result @?= (((), 4), 2)
        , let result = getResult $ myRunBatch $ (,) <$> s 5 4 <*> subtractNotification 20 16
          in testCase "batch with notification 2" $ result @?= ((1, ()), 2)
        ]

type Server = RpcResult (State Int)

singleSubtract :: Double -> Double -> Server Double
singleSubtract = toFunction myServer subtractSignature

getResult :: Server a -> (a, Int)
getResult s = runState (fromRight <$> runErrorT s) 0
    where fromRight (Right x) = x

myRunBatch :: A.FromJSON a => Batch a -> Server a
myRunBatch = runBatch myServer

subtractNotification = toBatchFunction_ subtractSignature

subtractSignature :: Signature (Double :+: Double :+: ()) Double
subtractSignature = Signature "subtract" (Param "x" :+: Param "y" :+: ())

s = toBatchFunction subtractSignature

myServer :: B.ByteString -> State Int B.ByteString
myServer = (fromJust <$>) . S.callWithBatchStrategy (sequence . reverse) methods

methods = S.toMethods [subtractMethod]

subtractMethod = S.toMethod "subtract" f params
    where params = S.Required "x" S.:+: S.Required "y" S.:+: ()
          f :: Double -> Double -> S.RpcResult (State Int) Double
          f x y = (x - y) <$ modify (+1)
