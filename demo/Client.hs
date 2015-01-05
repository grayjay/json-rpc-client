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
import Control.Monad.Error (runErrorT, liftIO)
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

type Result a = RpcResult (ReadInOut IO) a
type ReadInOut = ReaderT (Handle, Handle)

run :: Batch r -> Result r
run = runBatch connection

-- Define some client-side RPC functions from Signatures.
-- The type signatures aren't necessary:
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
connection input = do
  (inH, outH) <- ask
  liftIO $ B.hPutStrLn inH input
  liftIO $ hFlush inH
  line <- (head . B.lines) <$> liftIO (B.hGetContents outH)
  return $ if B.null line then Nothing else Just line

main = do
  cmd <- head <$> getArgs
  (inH, outH, _, processH) <- runInteractiveCommand cmd
  runReaderT (runErrorT runRpcs) (inH, outH)
  terminateProcess processH
