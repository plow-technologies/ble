{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main (main) where

-- This example contains a demonstration of the standard Heart Rate Service
-- (HRS). It serves as an examples of using notifications.
--
-- The program takes a command line argument to allow using controllers other
-- than the default (hci0). If you have multiple controllers on a single
-- machine (either hardware or virtual with something such as 'btvirt'), you
-- can thus test this service with the heart-rate client executable.
import Bluetooth
import Bluetooth.Internal.Interfaces (bluezPath)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Monoid
import Data.String                   (fromString)
import Data.Word                     (Word8)
import System.Log.Logger
import System.Random                 (randomRIO)

import qualified Data.ByteString     as BS
import qualified Options.Applicative as Opt


main :: IO ()
main = do
  options <- Opt.execParser opts
  writeIORef bluezPath $ fromString $ "/org/bluez/" ++ controller options
  when (verbose options) $ updateGlobalLogger rootLoggerName (setLevel DEBUG)
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
  { currentHeartRate :: IORef Word8
  }

data Options = Options
  { controller :: String
  , verbose    :: Bool
  }

opts :: Opt.ParserInfo Options
opts = Opt.info
  (parser Opt.<**> Opt.helper)
  (  Opt.fullDesc
  <> Opt.progDesc "Print a greeting for TARGET"
  <> Opt.header "hello - a test for optparse-applicative" )

parser :: Opt.Parser Options
parser = Options
  <$> Opt.strOption
     ( Opt.long "controller"
    <> Opt.metavar "HCIX"
    <> Opt.showDefault
    <> Opt.value "hci0"
    <> Opt.help "Controller to use")
  <*> Opt.switch
     ( Opt.long "verbose"
    <> Opt.short 'v'
    <> Opt.help "Print debugging info to console")

app :: AppState -> Application
app appState
  = "/com/turingjump/example/hrs"
    & services .~ [heartRateService appState]

heartRateService :: AppState -> Service 'Local
heartRateService appState
  = "0000180d-0000-1000-8000-00805f9b34fb"
    & characteristics .~ [heartRateMeasurement appState, bodySensorLocation]

heartRateMeasurement :: AppState -> CharacteristicBS 'Local
heartRateMeasurement appState
  = "00002a37-0000-1000-8000-00805f9b34fb"
     & readValue ?~ fmap heartRateToBS (liftIO readV)
     -- Even though we add a @writeValue@, this does not mean that the
     -- characteristic is writable (for that, we would need to add the CPWrite
     -- property to it). Instead, we can use the writeValue internally to
     -- update the value and send notifications each time the value is changed.
     & writeValue ?~ encodeWrite (liftIO <$> write)
     & properties .~ [ CPNotify, CPRead, CPWrite ]
  where
    readV = do
      putStrLn "Reading value"
      readIORef $ currentHeartRate appState

    write v = do
      putStrLn "Received value"
      writeIORef (currentHeartRate appState) v
      putStrLn "Wrote value"
      return True

heartRateToBS :: Word8 -> BS.ByteString
heartRateToBS i = BS.singleton 0x06 <> BS.singleton i

bodySensorLocation :: CharacteristicBS 'Local
bodySensorLocation
  = "00002a38-0000-1000-8000-00805f9b34fb"
     & readValue ?~ encodeRead (return (0x01 :: Word))
     & properties .~ [CPRead]
