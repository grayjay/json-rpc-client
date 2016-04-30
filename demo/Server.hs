{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Signatures (concatenateSig, incrementSig)
import Network.JsonRpc.Server (Method, call, toMethods)
import Network.JsonRpc.ServerAdapter (toServerMethod)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT, ask, runReaderT, liftIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)

-- This server uses an MVar to maintain a count
-- that can be read and updated by RPC calls:
type Server = ReaderT (MVar Int) IO

-- Create a Method from each Signature:
concatenate, increment :: Method Server

concatenate = toServerMethod concatenateSig (\x y -> return $ x ++ y)

increment = toServerMethod incrementSig $ ask >>= \count ->
            liftIO $ modifyMVar count inc
              where inc x = return (x + 1, x + 1)

-- Call the set of methods with requests from stdin,
-- and print responses to stdout:
main = do
  hSetBuffering stdout LineBuffering
  contents <- B.getContents
  count <- newMVar 0
  forM_ (B.lines contents) $ \request -> do
         response <- runReaderT (call methods request) count
         B.putStrLn $ fromMaybe "" response
      where methods = [concatenate, increment]
