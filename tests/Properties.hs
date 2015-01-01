{-# LANGUAGE OverloadedStrings,
             TypeOperators,
             MultiParamTypeClasses,
             UndecidableInstances,
             FlexibleContexts,
             FlexibleInstances #-}

module Properties (properties) where

import Network.JsonRpc.Client
import Network.JsonRpc.ServerAdapter
import Network.JsonRpc.Server
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad.Error (ErrorT (..), runErrorT, throwError)
import Control.Monad.State (State, runState, evalState, gets, put, modify)
import Data.Text (Text, pack)
import Data.List (nub)
import Data.Traversable (traverse)
import Control.Applicative (pure, (<$>), (<*>))
import Test.QuickCheck( Arbitrary (..), CoArbitrary (..), Blind (..)
                      , Property, Gen, listOf, oneof, property, (===), (==>))
import Test.QuickCheck.Gen.Unsafe (promote)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

properties :: [Test]
properties = [ testProperty "rpc vs. direct call" prop_rpcVsDirect
             , testProperty "single vs. batch" prop_singleVsBatch
             , testProperty "batch functor id law" prop_functorId
             , testProperty "batch functor composition law" prop_functorComposition
             , testProperty "batch applicative id law" prop_applicativeId
             , testProperty "batch applicative composition law" prop_applicativeComposition
             , testProperty "batch applicative homomorphism law" prop_applicativeHomomorphism
             , testProperty "batch applicative interchange law" prop_applicativeInterchange
             , testProperty "no unexpected errors" prop_noUnexpectedErrors ]

type A = Int
type B = Double
type C = Maybe Int
type D = (Bool, Int)
type E = Integer
type S = Int

prop_rpcVsDirect :: Signature (A ::: B ::: ()) C
                 -> Blind (A -> B -> RpcResult (State D) C)
                 -> A -> B -> D -> Property
prop_rpcVsDirect sig@(Signature _ ps) (Blind f) x y state = unique (paramNames ps) ==>
                                                            run (f x y) === run (rpcFunction x y)
    where server = call $ toMethods [toServerMethod sig f]
          rpcFunction = toFunction server sig
          run result = runState (runErrorT result) state

-- A sequence of requests should yield the same result whether batched or
-- sent individually in the State monad, if the server evaluates the
-- requests sequentially.  The state differs because the server processes
-- all requests in a batch, but the client can stop sending single requests
-- after the first failure.
prop_singleVsBatch :: Signature (A ::: B ::: ()) C
                   -> Blind (A -> B -> RpcResult (State D) C)
                   -> [(A, B)] -> D -> Property
prop_singleVsBatch sig (Blind f) args state = let server = call $ toMethods [toServerMethod sig f]
                                                  function = toFunction server sig
                                                  functionB = toBatchFunction sig
                                                  run result = evalState (runErrorT result) state
                                              in run (mapM (uncurry function) args) ===
                                                 run (runBatch server $ traverse (uncurry functionB) args)

type Sigs = Signature (A ::: B ::: ()) C
        :*: Signature () D
        :*: Signature (E ::: D ::: C ::: B ::: ()) A

prop_functorId :: Sigs
               -> ToServer Sigs S
               -> ToBatch Sigs A
               -> S
               -> Property
prop_functorId sigs toServer toBatchX state = run (fmap id x) === run (id x)
    where x = getBatch toBatchX sigs
          run = myRunBatch toServer sigs state

prop_functorComposition :: Sigs
                        -> ToServer Sigs S
                        -> Blind (B -> C)
                        -> Blind (A -> B)
                        -> ToBatch Sigs A
                        -> S
                        -> Property
prop_functorComposition sigs toServer (Blind f) (Blind g) toBatchX state =
                            run (fmap (f . g) x) === run (fmap f . fmap g $ x)
    where x = getBatch toBatchX sigs
          run = myRunBatch toServer sigs state

prop_applicativeId :: Sigs
                   -> ToServer Sigs S
                   -> ToBatch Sigs A
                   -> S
                   -> Property
prop_applicativeId sigs toServer toBatch state = run (pure id <*> v) === run v
    where v = getBatch toBatch sigs
          run = myRunBatch toServer sigs state

prop_applicativeComposition :: Sigs
                            -> ToServer Sigs S
                            -> ToBatch Sigs (B -> C)
                            -> ToBatch Sigs (A -> B)
                            -> ToBatch Sigs A
                            -> S
                            -> Property
prop_applicativeComposition sigs toServer toBatchU toBatchV toBatchW state =
                                      run (pure (.) <*> u <*> v <*> w) ===
                                      run (u <*> (v <*> w))
    where u = getBatch toBatchU sigs
          v = getBatch toBatchV sigs
          w = getBatch toBatchW sigs
          run = myRunBatch toServer sigs state

prop_applicativeHomomorphism :: Sigs
                             -> ToServer Sigs S
                             -> Blind (A -> B)
                             -> A
                             -> S
                             -> Property
prop_applicativeHomomorphism sigs toServer (Blind f) x state =
                                      run (pure f <*> pure x) ===
                                      run (pure (f x))
    where run = myRunBatch toServer sigs state

prop_applicativeInterchange :: Sigs
                            -> ToServer Sigs S
                            -> ToBatch Sigs (A -> B)
                            -> A
                            -> S
                            -> Property
prop_applicativeInterchange sigs toServer toBatchU y state =
                                      run (u <*> pure y) ===
                                      run (pure ($ y) <*> u)
    where u = getBatch toBatchU sigs
          run = myRunBatch toServer sigs state

prop_noUnexpectedErrors :: Sigs
                        -> ToServer Sigs S
                        -> ToBatch Sigs A
                        -> S
                        -> Property
prop_noUnexpectedErrors sigs toServer toBatch state = unique (methodNames sigs) ==>
                                                    all unique (allParamNames sigs)  ==>
                                                    case run batch of
                                                      (Left err, _) -> err === testError
                                                      _ -> property True
    where batch = getBatch toBatch sigs
          run = myRunBatch toServer sigs state

unique xs = nub xs == xs

myRunBatch toServer sigs state result = let server = getServer toServer sigs
                                        in runState (runErrorT $ runBatch server result) state

data a :*: b = a :*: b deriving Show
infixr :*:

class SignatureSet ss where
    methodNames :: ss -> [Text]
    allParamNames :: ss -> [[Text]]
    batchFromSigs :: Arbitrary a => ss -> Gen (Batch a)
    toServerMethods :: ss -> Gen [Method (State S)]

instance (TestClientFunction ps r f1, Params ps,
          MethodParams f2 ps2 (State S) r, ConvertParams ps ps2, Arbitrary f2, Arbitrary r, CoArbitrary r)
         => SignatureSet (Signature ps r) where
    methodNames (Signature name _) = [name]
    allParamNames (Signature _ ps) = [paramNames ps]
    batchFromSigs = batchFromSig
    toServerMethods sig = (\f -> [toServerMethod sig f]) <$> arbitrary

instance (SignatureSet ss, TestClientFunction ps r f1, Params ps,
          MethodParams f2 ps2 (State S) r, ConvertParams ps ps2, Arbitrary f2, Arbitrary r, CoArbitrary r)
         => SignatureSet (Signature ps r :*: ss) where
    methodNames (Signature name _ :*: sigs) = name : methodNames sigs
    allParamNames (Signature _ ps :*: sigs) = paramNames ps : allParamNames sigs
    batchFromSigs (sig :*: sigs) = oneof [batchFromSig sig, batchFromSigs sigs]
    toServerMethods (sig :*: sigs) = combine <$> arbitrary <*> toServerMethods sigs
        where combine f sm = toServerMethod sig f : sm

batchFromSig sig = ((<$>) <$> arbitrary) <*> arbitraryFunctionCall (toBatchFunction sig)

newtype ToBatch ss r = ToBatch { getBatch :: ss -> Batch r }

instance Show (ToBatch ss r) where
    show _ = "ToBatch"

instance (SignatureSet ss, Arbitrary r) => Arbitrary (ToBatch ss r) where
    arbitrary = ToBatch <$> oneof
                [ promote batchFromSigs
                , promote $ combine <$> batchFromSigs <*> (batchFromSigs :: SignatureSet ss => ss -> Gen (Batch String))]
        where combine x y = (<*>) <$> x <*> y

instance (Arbitrary s, Arbitrary ss) => Arbitrary (s :*: ss) where
    arbitrary = (:*:) <$> arbitrary <*> arbitrary

newtype ToServer ss s = ToServer { getServer :: ss -> Server (State s) }

instance Show (ToServer ss s) where
    show _ = "ToServer"

instance SignatureSet ss => Arbitrary (ToServer ss S) where
    arbitrary = ToServer <$> promote (\ss -> (call . toMethods) <$> toServerMethods ss)

testError = rpcError 9999 "Test error"

instance (Arbitrary a, Arbitrary s, CoArbitrary s) =>
    Arbitrary (RpcResult (State s) a) where
        arbitrary = (>>) <$> (sequence <$> stateSeq) <*> oneof stateEnd
            where stateEnd = [ return (throwError testError), gets <$> arbitrary ]
                  stateSeq = listOf $ oneof [ put <$> arbitrary
                                            , modify <$> arbitrary ]

instance Arbitrary ps => Arbitrary (p ::: ps) where
    arbitrary = (:::) <$> (pack <$> arbitrary) <*> arbitrary

instance Arbitrary ps => Arbitrary (Signature ps r) where
    arbitrary = Signature <$> (pack <$> arbitrary) <*> arbitrary

class (ClientFunction ps r f, Arbitrary r, FromJSON r) => TestClientFunction ps r f where
    arbitraryFunctionCall :: f -> Gen (Batch r)

instance (Arbitrary r, FromJSON r) => TestClientFunction () r (Batch r) where
    arbitraryFunctionCall = return

instance (TestClientFunction ps r f, Arbitrary a, ToJSON a)
    => TestClientFunction (a ::: ps) r (a -> f) where
    arbitraryFunctionCall f = arbitraryFunctionCall =<< (f <$> arbitrary)

class Params ps where
    paramNames :: ps -> [Text]

instance Params () where
    paramNames _ = []

instance Params ps => Params (p ::: ps) where
    paramNames (p ::: ps) = p : paramNames ps
