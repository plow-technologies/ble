module Main (main) where

import Bluetooth
import Data.Maybe
{-import Control.Monad.IO.Class-}
import Control.Concurrent
import Data.Bits          (testBit)

import qualified Data.ByteString       as BS
{-import qualified Data.ByteString.Char8 as BSC-}
import qualified Data.Serialize        as S

import Debug.Trace

-- This example contains a demonstration of a client that interacts with a
-- Heart Rate Service.

main :: IO ()
main = do
  res <- connect >>= runBluetoothM go
  threadDelay 10000000
  case res of
    Left e -> error $ show e
    Right () -> return ()

  where
    go = do
      mhrs <- getService "0000180d-0000-1000-8000-00805f9b34fb"


      let mhandlers = case mhrs of
            Nothing  -> error "No heart rate service found!"
            Just hrs -> do
              traceShowM $ fmap (^. uuid) $ hrs ^. characteristics
              {-bodyLocChar <- listToMaybe $ filter-}
                {-(\x -> x ^. uuid == bodySensorLocationUUID)-}
                {-(hrs ^. characteristics)-}
              {-readBodyLoc <- bodyLocChar ^. readValue-}
              measurementChar <- listToMaybe $ filter
                (\x -> x ^. uuid == heartRateMeasurementUUID)
                (hrs ^. characteristics)
              {-readMeasurement <- measurementChar ^. readValue-}
              {-return (readBodyLoc, readMeasurement)-}
              return measurementChar

      case mhandlers of
        Nothing -> error "Couldn't find expected characteristics, or they're not readable"
        {-Just (rbodyLoc, rmeasurement) -> do-}
          {-liftIO $ putStrLn "Reading body location"-}
          {-bodyLoc <- rbodyLoc-}
          {-liftIO $ print bodyLoc-}

          {-liftIO $ putStrLn "Reading heart rate"-}
          {-measurement <- rmeasurement-}
          {-liftIO $ print measurement-}

        Just rmeasurement -> do
          startNotify rmeasurement $ \x -> case deserializeMeasurement x of
            Nothing -> putStrLn "Error decoding value"
            Just v  -> putStrLn $ "Received Heart Rate: " ++ show v
          {-liftIO $ print measurement-}

-- * Types

-- Heart rate in bpm
newtype HeartRate = HeartRate Integer
  deriving (Eq, Show, Read)

-- See https://www.bluetooth.com/specifications/gatt/viewer?attributeXmlFile=org.bluetooth.characteristic.heart_rate_measurement.xml
deserializeMeasurement :: BS.ByteString -> Maybe HeartRate
deserializeMeasurement bs = do
  (flags, dat) <- BS.uncons bs
  if testBit flags 0
    -- Uses uint8
    then HeartRate . toInteger . fst <$> BS.uncons dat
    -- Uses uint16
    else case S.decode $ BS.take 2 dat of
      Left _ -> Nothing
      Right v -> Just $ HeartRate v

-- * Constants



heartRateMeasurementUUID :: UUID
heartRateMeasurementUUID = "00002a37-0000-1000-8000-00805f9b34fb"

{-bodySensorLocationUUID :: UUID-}
{-bodySensorLocationUUID = "00002a38-0000-1000-8000-00805f9b34fb"-}
