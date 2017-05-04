module Mock where

import           Bluetooth               (UUID)
import           Bluetooth.Internal.DBus (bluezName, bluezPath)
import           Data.IORef
import           Paths_ble
import           System.IO
import qualified System.Process          as P

withAService :: (UUID -> IO a) -> IO a
withAService x = do
  writeIORef bluezName "org.bluez.Mock"
  writeIORef bluezPath "/org/bluez/hci0"
  let uuid = "4ea7235c-8d49-4a6f-abe6-1883218a93a7"
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  file <- getDataFileName "test/Mock/start_mock.sh"
  (_inHandle, Just outHandle, _errHandle, proc)
    <- P.createProcess (P.shell $ "bash " ++ file) { P.std_out = P.CreatePipe }
  _ <- hGetLine outHandle
  val <- x uuid
  P.terminateProcess proc
  return val
