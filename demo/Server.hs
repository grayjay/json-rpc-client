{-# LANGUAGE OverloadedStrings,
             TypeOperators #-}

module Main (main) where

import Signatures (concatenateSig, incrementSig)
import Network.JsonRpc.Server (Methods, Method, call, toMethods)
import Network.JsonRpc.ServerAdapter (toServerMethod)
import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT, ask, runReaderT, liftIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)

main = do
  contents <- B.getContents
  count <- newMVar 0
  forM_ (B.lines contents) $ \request -> do
         response <- runReaderT (call methods request) count
         B.putStrLn $ fromMaybe "" response
         hFlush stdout

type Server = ReaderT (MVar Int) IO

methods :: Methods Server
methods = toMethods [concatenate, increment]

concatenate, increment :: Method Server
concatenate = toServerMethod concatenateSig (\x y -> return $ x ++ y)

increment = toServerMethod incrementSig $ ask >>= \count ->
            liftIO $ modifyMVar count inc
              where inc x = return (x + 1, x + 1)
