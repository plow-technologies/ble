{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main (main) where

-- This example contains a demonstration of the standard Heart Rate Service
-- (HRS). It serves as an examples of using notifications.
import Bluetooth
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Monoid
import System.Random          (randomRIO)
import System.Log.Logger

import qualified Data.Serialize as S
import qualified Data.ByteString as BS


main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  heartRateRef <- newIORef 0
  let s = AppState heartRateRef
  conn <- connect
  x <- runBluetoothM (registerAndAdvertiseApplication $ app s) conn
  registered <- case x of
    Right registered -> do
      putStrLn "Started BLE Heart Rate Service application!"
      return registered
    Left e -> error $ "Error starting application" ++ show e
  forever $ do
    newValue <- randomRIO (50, 150)
    writeIORef heartRateRef newValue
    res <- runBluetoothM (triggerNotification registered $ heartRateMeasurement s) conn
    case res of
      Right ()  -> putStrLn "Notification sent!"
      Left  e     -> error $ "Bluetooth error:\n" ++ show e
    -- We update the value every ten seconds
    threadDelay (10 ^ 7)

data AppState = AppState
  { currentHeartRate :: IORef Int
  }

app :: AppState -> Application
app appState
  = "/com/turingjump/example/hrs"
    & services .~ [heartRateService appState]

heartRateService :: AppState -> Service Handler
heartRateService appState
  = "0000180d-0000-1000-8000-00805f9b34fb"
    & characteristics .~ [heartRateMeasurement appState, bodySensorLocation]

heartRateMeasurement :: AppState -> CharacteristicBS Handler
heartRateMeasurement appState
  = "00002a37-0000-1000-8000-00805f9b34fb"
     & readValue ?~ fmap heartRateToBS (liftIO . readIORef $ currentHeartRate appState)
     -- Even though we add a @writeValue@, this does not mean that the
     -- characteristic is writable (for that, we would need to add the CPWrite
     -- property to it). Instead, we can use the writeValue internally to
     -- update the value and send notifications each time the value is changed.
     & writeValue ?~ encodeWrite (liftIO <$> write)
     & properties .~ [CPNotify, CPRead]
  where
    write v = do
      writeIORef (currentHeartRate appState) v
      return True

heartRateToBS :: Int -> BS.ByteString
heartRateToBS i = "0x06" <> S.encode i

bodySensorLocation :: CharacteristicBS Handler
bodySensorLocation
  = "00002a38-0000-1000-8000-00805f9b34fb"
     & readValue ?~ encodeRead (return (0x01 :: Word))
     & properties .~ [CPRead]
