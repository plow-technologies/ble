module Mock where

import           Bluetooth            (UUID)
{-import           GHC.IO.Handle        (hGetLine)-}
import           Paths_ble
import qualified System.Process.Typed as P
import System.IO

import Control.Concurrent

withAService :: (UUID -> IO a) -> IO a
withAService x = do
  let uuid = "4ea7235c-8d49-4a6f-abe6-1883218a93a7"
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  file <- getDataFileName "test/Mock/start_mock.sh"
  putStrLn "starting proc"
  proc <- P.startProcess $ P.shell $ "bash " ++ file
  putStrLn "waiting for proc to be ready "
  threadDelay $ 5 * 10^(6 :: Int)
  val <- x uuid
  putStrLn "ending proc"
  P.stopProcess proc
  return val
