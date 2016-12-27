module Main (main) where

import Bluetooth



app :: IORef Int -> Application
app ref = _

heartRateService :: IORef Int -> Service
heartRateService
  = "0000180d-0000-1000-8000-00805f9b34fb"
    & characteristics .~ [heartRateMeasurement, bodySensorLocation]

heartRateMeasurement :: IORef Int -> CharacteristicBS
heartRateMeasurement
  = "00002a37-0000-1000-8000-00805f9b34fb"
     & properties .~ [CPNotify]

bodySensorLocation :: CharacteristicBS
bodySensorLocation
  = "00002a38-0000-1000-8000-00805f9b34fb"
     & readValue ?~ encodedRead (0x01 :: Word)
     & properties .~ [CPRead, CPWrite]
