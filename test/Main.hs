module Main (main) where

import System.Log.Logger
import System.Log.Handler.Simple
import System.IO
import qualified Spec
import Test.Hspec

main :: IO ()
main = do
  h <- streamHandler stderr DEBUG
  updateGlobalLogger rootLoggerName (addHandler h)
  hspec Spec.spec
