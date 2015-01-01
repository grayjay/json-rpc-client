module Main (main) where

import qualified Tests
import qualified Properties
import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain $ Properties.properties ++ Tests.tests
