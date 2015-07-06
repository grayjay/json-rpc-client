{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Signatures (concatenateSig, incrementSig)
import Network.JsonRpc.Client
import System.Process (runInteractiveCommand, terminateProcess)
import System.IO (Handle, hFlush)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Traversable (sequenceA)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Except (runExceptT, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

runRpcs :: Result ()
runRpcs = do
  -- Send one request (prints 1):
  printResult =<< increment

  -- Send a notification:
  increment_

  -- Batch two requests (prints (3, "abcxyz")):
  printResult =<< run ((,) <$> incrementB <*> concatenateB "abc" "xyz")

  -- Create a batch with three requests:
  let inc3 = sequenceA $ replicate 3 incrementB

  -- Run the batch (prints [4,5,6]):
  printResult =<< run inc3

  -- Run the batch as three notifications:
  run $ voidBatch inc3

  -- Send two single requests (prints "count=10"):
  printResult =<< concatenate "count=" . show =<< increment
      where printResult x = liftIO $ print x

-- This client's RPC calls need access to the stdin
-- and stdout handles of the server subprocess:
type Result a = RpcResult (ReadInOut IO) a
type ReadInOut = ReaderT (Handle, Handle)

run :: Batch r -> Result r
run = runBatch connection

-- Define some client-side RPC functions from
-- the imported Signatures.
concatenate :: String -> String -> Result String
concatenate = toFunction connection concatenateSig

concatenateB :: String -> String -> Batch String
concatenateB = toBatchFunction concatenateSig

increment :: Result Int
increment = toFunction connection incrementSig

increment_ :: Result ()
increment_ = toFunction_ connection incrementSig

incrementB :: Batch Int
incrementB = toBatchFunction incrementSig

-- Create a function for communicating with the server:
connection :: Connection (ReadInOut IO)
connection input = ask >>= \(inH, outH) -> liftIO $
                   do B.hPutStrLn inH input
                      hFlush inH
                      line <- (head . B.lines) <$> B.hGetContents outH
                      return $ if B.null line
                               then Nothing
                               else Just line

-- Run the server as a subprocess:
main = do
  cmd <- head <$> getArgs
  (inH, outH, _, processH) <- runInteractiveCommand cmd
  runReaderT (runExceptT runRpcs) (inH, outH)
  terminateProcess processH
