module Mock where

import           Bluetooth               (UUID)
import           Bluetooth.Internal.DBus (bluezName, bluezPath)
import           Data.IORef
import           Paths_ble
import           System.IO
import qualified System.Process          as P

import Control.Concurrent

withAService :: (UUID -> IO a) -> IO a
withAService x = do
  writeIORef bluezName "org.bluez.Mock"
  writeIORef bluezPath "/org/bluez/hci0"
  let uuid = "4ea7235c-8d49-4a6f-abe6-1883218a93a7"
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  file <- getDataFileName "test/Mock/start_mock.sh"
  putStrLn "starting proc"
  proc <- P.spawnCommand ("bash " ++ file)
  putStrLn "waiting for proc to be ready "
  threadDelay $ 5 * 10^(6 :: Int)
  val <- x uuid
  putStrLn "ending proc"
  P.terminateProcess proc
  return val
