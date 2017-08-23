module Main (main) where

import Bluetooth
import Data.Maybe
{-import Control.Monad.IO.Class-}
import qualified Data.ByteString.Char8 as BSC
import Control.Concurrent

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

        Just (rmeasurement) -> do
          {-liftIO $ putStrLn "Reading heart rate"-}
          startNotify rmeasurement BSC.putStrLn
          {-liftIO $ print measurement-}

-- * Constants

heartRateMeasurementUUID :: UUID
heartRateMeasurementUUID = "00002a37-0000-1000-8000-00805f9b34fb"

{-bodySensorLocationUUID :: UUID-}
{-bodySensorLocationUUID = "00002a38-0000-1000-8000-00805f9b34fb"-}
