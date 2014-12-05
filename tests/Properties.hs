{-# LANGUAGE TypeOperators,
             FlexibleInstances #-}

module Properties (properties) where

import Network.JsonRpc.Client
import Network.JsonRpc.ServerAdapter
import Network.JsonRpc.Server (call, toMethods, rpcError)
import Control.Monad.Error (ErrorT (..), runErrorT)
import Control.Monad.State (State, runState, evalState, gets, put, modify)
import Data.Text (Text, pack)
import Data.List (nub)
import Data.Traversable (traverse)
import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Blind (..), Prop, Gen, listOf, oneof, (==>))
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

properties :: [Test]
properties = [ testProperty "rpc vs. direct call" prop_rpcVsDirect
             , testProperty "single vs. batch" prop_singleVsBatch ]

type A = Int
type B = Double
type C = Integer
type D = (Bool, Int)

prop_rpcVsDirect :: Signature (A ::: B ::: ()) C
                 -> Blind (A -> B -> RpcResult (State D) C)
                 -> A -> B -> D -> Gen Prop
prop_rpcVsDirect sig@(Signature _ ps) (Blind f) x y state = (nub params == params) ==>
                                                            (run (f x y) == run (rpcFunction x y))
    where server = call $ toMethods [toServerMethod sig f]
          rpcFunction = toFunction server sig
          run result = runState (runErrorT result) state
          params = getParams ps

-- A sequence of requests should yield the same result whether batched or
-- sent individually in the State monad, if the server evaluates the
-- requests sequentially.  The state differs because the server processes
-- all requests in a batch, but the client can stop sending single requests
-- after the first failure.
prop_singleVsBatch :: Signature (A ::: B ::: ()) C
                   -> Blind (A -> B -> RpcResult (State D) C)
                   -> [(A, B)] -> D -> Bool
prop_singleVsBatch sig (Blind f) args state = let server = call $ toMethods [toServerMethod sig f]
                                                  function = toFunction server sig
                                                  functionB = toBatchFunction sig
                                                  run result = evalState (runErrorT result) state
                                              in run (mapM (uncurry function) args) ==
                                                 run (runBatch server $ traverse (uncurry functionB) args)

instance Arbitrary RpcError where
    arbitrary = rpcError <$> arbitrary <*> (pack <$> arbitrary)

instance (Arbitrary a, Arbitrary s, CoArbitrary s) =>
    Arbitrary (RpcResult (State s) a) where
        arbitrary = (>>) <$> (sequence <$> stateSeqGen) <*> oneof stateEnd
            where stateEnd = [ (ErrorT . return) <$> arbitrary, gets <$> arbitrary ]
                  stateSeqGen = listOf $ oneof [ put <$> arbitrary
                                               , modify <$> arbitrary ]

instance Arbitrary ps => Arbitrary (p ::: ps) where
    arbitrary = (:::) <$> (pack <$> arbitrary) <*> arbitrary

instance Arbitrary ps => Arbitrary (Signature ps r) where
    arbitrary = Signature <$> (pack <$> arbitrary) <*> arbitrary

class GetParams ps where
    getParams :: ps -> [Text]

instance GetParams () where
    getParams _ = []

instance GetParams ps => GetParams (p ::: ps) where
    getParams (p ::: ps) = p : getParams ps
