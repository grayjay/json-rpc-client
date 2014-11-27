{-# LANGUAGE OverloadedStrings,
             PackageImports,
             TypeOperators #-}

module Main (main) where

import Network.JsonRpc.Client
import Network.JsonRpc.Common
import qualified "json-rpc-server" Network.JsonRpc.Server as S
import Control.Applicative
import Data.Maybe (fromJust)
import Control.Monad.Error
import Test.HUnit hiding (State, Test)
import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testCase "single" $ getResult (singleSubtract 22 1) @?= 21
        , testCase "batch length 1" $ getResult (myRunBatch $ s 1 2) @?= (-1)
        , let result = getResult $ myRunBatch $ (+) <$> s 5 3 <*> ((*) <$> s 11 2 <*> s 15 10)
          in testCase "batch 2" $ result @?= 47
        , let result = getResult $ myRunBatch $ (*) <$> ((+) <$> s 5 3 <*> s 11 2) <*> s 15 10
          in testCase "batch 3" $ result @?= 55
        ]

singleSubtract = createMonadFunction subtractSignature myServer

getResult = fromRight . fromJust . runErrorT
    where fromRight (Right x) = x

myRunBatch = runBatch myServer

subtractSignature :: Signature (Double :+: Double :+: ()) Double
subtractSignature = signature "subtract" (Param "x" :+: Param "y" :+: ())

s = createClientFunction subtractSignature

myServer = fromJust . S.callWithBatchStrategy (sequence . reverse) methods

methods = S.toMethods [subtractMethod]

subtractMethod = S.toMethod "subtract" f params
    where params = S.Required "x" S.:+: S.Required "y" S.:+: ()
          f :: Double -> Double -> S.RpcResult Maybe Double
          f x y = return $ x - y
