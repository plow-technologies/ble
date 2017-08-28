module Mock where

import           Bluetooth
import           Bluetooth.Internal.Interfaces (bluezPath)
import           Data.IORef
{-import           Paths_ble-}
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Data.ByteString as BS
{-import qualified System.Process                as P-}

withAService :: IO a -> IO a
withAService action = action `whileRunning` (do
  app' <- app
  conn <- connect
  registered <- runBluetoothM (registerAndAdvertiseApplication app') conn
  case registered of
    Right _ -> return ()
    Left e -> error $ show e)


whileRunning :: IO a -> IO () -> IO a
whileRunning onThisController onThatController = do
  writeIORef bluezPath "/org/bluez/hci1"
  thatThread <- forkIO onThatController
  threadDelay 100000000
  writeIORef bluezPath "/org/bluez/hci0"
  result <- onThisController
  killThread thatThread
  return result

app :: IO Application
app = do
  ref <- newIORef "blah"
  return $ "/com/turingjump/example/app0"
    & services .~ [someService ref]

someService :: IORef BS.ByteString -> Service 'Local
someService ref
  = "4ea7235c-8d49-4a6f-abe6-1883218a93a7"
    & characteristics .~ [someCharacteristic ref]

someCharacteristic :: IORef BS.ByteString -> CharacteristicBS 'Local
someCharacteristic ref
  = "6fe4afc7-ebf8-4369-90aa-0fe45064e3f9"
     & readValue ?~ liftIO (readIORef ref)
     & writeValue ?~ fmap liftIO write
     & properties .~ [CPNotify, CPWrite, CPRead]
  where
    write v = do
      writeIORef ref v
      return True


mockServiceUUID :: UUID
mockServiceUUID = "4ea7235c-8d49-4a6f-abe6-1883218a93a7"

mockCharUUID :: UUID
mockCharUUID = "6fe4afc7-ebf8-4369-90aa-0fe45064e3f9"

mockCharValue :: Int
mockCharValue = 1797
