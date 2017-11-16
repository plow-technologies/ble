{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Bluetooth
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Bits          (testBit)
import Data.Word          (Word16)
import Data.List
import qualified Data.ByteString as BS
import qualified Data.Serialize  as S
import Data.Monoid
import DBus.Types
import Data.Text (Text)
-- This example contains a demonstration of a client that interacts with a
-- Heart Rate Service.


michaelsLaptop :: Text
michaelsLaptop = "dev_A0_2C_36_89_BE_8F" 

main :: IO ()
main = do
  res <- connect >>= runBluetoothM go  
  case res of
    Left e -> error $ show e
    Right svcs -> print "Right received" >> (print . fmap serviceUuid $ svcs) >> threadDelay 1000

  where
    go = do

      !devices <- getAllDevices
      liftIO $ print devices
      let (laptopFromSearch: []) = filter (\d -> (last . opParts . devicePath $ d) ==
                                                 (michaelsLaptop) ) devices
      liftIO $ print "Connecting to laptop"                             
      connectTo laptopFromSearch
      getAllServices

      -- liftIO $ print "Connected"
      -- !mhrs <- getService "0000180d-0000-1000-8000-00805f9b34fb"
      -- !svcs <- getDeviceServiceUUIDs laptopFromSearch
      -- liftIO $ print "service retrieved"
      -- liftIO $ print $ "all services null is: " <> (show $ null svcs)
    --   let handler = case mhrs of
    --         Nothing  -> error $ "no conn"
    --         Just hrs -> case filter
    --           (\x -> x ^. uuid == heartRateMeasurementUUID)
    --           (hrs ^. characteristics) of
    --           [] -> error "Couldn't find expected characteristics"
    --           measurementChar:_ -> measurementChar
    --   handleNotify handler

    -- handleNotify rmeasurement = startNotify rmeasurement $
    --   \x -> case deserializeMeasurement x of
    --     Nothing -> putStrLn "Error decoding value"
    --     Just v  -> do
    --       putStrLn $ "Received Heart Rate: " ++ show v

-- * Types

-- Heart rate in bpm
newtype HeartRate = HeartRate Integer
  deriving (Eq, Show, Read)

-- See https://www.bluetooth.com/specifications/gatt/viewer?attributeXmlFile=org.bluetooth.characteristic.heart_rate_measurement.xml
deserializeMeasurement :: BS.ByteString -> Maybe HeartRate
deserializeMeasurement bs = do
  (flags, dat) <- BS.uncons bs
  if not $ testBit flags 0
    -- Uses uint8
    then HeartRate . toInteger . fst <$> BS.uncons dat
    -- Uses uint16
    else case S.decode $ BS.take 2 dat of
      Left e -> error $ show e
      Right (v :: Word16) -> Just $ HeartRate $ toInteger v

-- * Constants
heartRateMeasurementUUID :: UUID
heartRateMeasurementUUID = "00002a37-0000-1000-8000-00805f9b34fb"
