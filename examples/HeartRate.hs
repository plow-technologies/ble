{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main (main) where

-- This example contains a demonstration of the standard Heart Rate Service
-- (HRS). It serves as an examples of using notifications. (NOT YET FUNCTIONAL)
import Bluetooth
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import System.Random          (randomRIO)


main :: IO ()
main = do
  heartRateRef <- newIORef 0
  isNotifyingRef <- newIORef False
  let s = AppState heartRateRef isNotifyingRef
  conn <- connect
  x <- runBluetoothM (registerAndAdvertiseApplication $ app s) conn
  case x of
    Right () -> putStrLn "Started BLE Heart Rate Service application!"
    Left e -> error $ "Error starting application" ++ show e
  forever $ do
    newValue <- randomRIO (50, 150)
    {-writeChrc (heartRateMeasurement s) newValue-}
    writeIORef heartRateRef newValue
    putStrLn "Value updated!"
    nots <- readIORef isNotifyingRef
    putStrLn $ if nots then "Notifying" else "Not notifying"
    -- We update the value every ten seconds
    threadDelay (10 ^ 7)

data AppState = AppState
  { currentHeartRate :: IORef Int
  , isNotifying      :: IORef Bool
  }

app :: AppState -> Application
app appState
  = "/com/turingjump/example/hrs"
    & services .~ [heartRateService appState]

heartRateService :: AppState -> Service
heartRateService appState
  = "0000180d-0000-1000-8000-00805f9b34fb"
    & characteristics .~ [heartRateMeasurement appState, bodySensorLocation]

heartRateMeasurement :: AppState -> CharacteristicBS
heartRateMeasurement appState
  = "00002a37-0000-1000-8000-00805f9b34fb"
     & readValue ?~ encodeRead (liftIO . readIORef $ currentHeartRate appState)
     -- Even though we add a @writeValue@, this does not mean that the
     -- characteristic is writable (for that, we would need to add the CPWrite
     -- property to it). Instead, we can use the writeValue internally to
     -- update the value and send notifications each time the value is changed.
     & writeValue ?~ encodeWrite (liftIO <$> write)
     & properties .~ [CPNotify, CPRead]
     & notifying ?~ isNotifying appState
  where
    write v = do
      writeIORef (currentHeartRate appState) v
      return True

bodySensorLocation :: CharacteristicBS
bodySensorLocation
  = "00002a38-0000-1000-8000-00805f9b34fb"
     & readValue ?~ encodeRead (return (0x01 :: Word))
     & properties .~ [CPRead]
