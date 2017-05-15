module Mock where

import           Bluetooth                     (UUID)
import           Bluetooth.Internal.Interfaces (bluezName, bluezPath)
import           Data.IORef
import           Paths_ble
import           System.IO
import qualified System.Process                as P

withAService :: IO a -> IO a
withAService action = do
  writeIORef bluezName "org.bluez.Mock"
  writeIORef bluezPath "/org/bluez/hci0"
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  file <- getDataFileName "test/Mock/start_mock.sh"
  (_inHandle, Just outHandle, _errHandle, proc)
    <- P.createProcess (P.shell $ "bash " ++ file) { P.std_out = P.CreatePipe }
  _ <- hGetLine outHandle
  val <- action
  P.terminateProcess proc
  return val

mockServiceUUID :: UUID
mockServiceUUID = "4ea7235c-8d49-4a6f-abe6-1883218a93a7"

mockCharUUID :: UUID
mockCharUUID = "6fe4afc7-ebf8-4369-90aa-0fe45064e3f9"

mockCharValue :: Int
mockCharValue = 1797
