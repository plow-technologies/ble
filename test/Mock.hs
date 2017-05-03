module Mock where

import Bluetooth (UUID)
import System.Process.Typed
import Paths_ble

withHRSService :: (UUID -> IO a) -> IO a
withHRSService x = do
  let uuid = "4ea7235c-8d49-4a6f-abe6-1883218a93a7"
  file <- getDataFileName "start_mock.sh"
  withProcess (shell file) (const $ x uuid)

