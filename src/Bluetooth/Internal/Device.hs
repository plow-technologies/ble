module Bluetooth.Internal.Device where

import Control.Monad.Except   (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader   (ask)
import Data.IORef             (readIORef)
import DBus                   (DBusType, RemoteProperty (..), Representable,
                               callMethod, getProperty, setProperty, ObjectPath)
import Lens.Micro

import qualified Data.Text as T
import qualified Data.Map as Map

import Bluetooth.Internal.Interfaces
import Bluetooth.Internal.Lenses
import Bluetooth.Internal.Types

-- | Retrieves the list of all known devices. This does not by itself do device
-- discovery; instead, it just lists devices that have already been discovered.
--
-- Usually device discovery happens at regular intervals and automatically.
getAllDevices :: BluetoothM [Device]
getAllDevices = do
  conn <- ask
  bluezName' <- liftIO $ readIORef bluezName
  objects :: Map.Map ObjectPath (Map.Map T.Text (Map.Map T.Text DontCareFromRep))
    <- toBluetoothM . const
    $ callMethod bluezName' "/" (T.pack objectManagerIFace) "GetManagedObjects" ()
      [] (dbusConn conn)
  let devices =
       Map.keys $ Map.filter (T.pack deviceIFace `Map.member`) objects
  return $ Device <$> devices


-- Prepositional bestiary

-- | Connect to a bluetooth device
connectTo :: Device -> BluetoothM ()
connectTo dev = callDeviceMethod dev "Connect"

-- | Disconnect from a bluetooth device
disconnectFrom :: Device -> BluetoothM ()
disconnectFrom dev = callDeviceMethod dev "Disonnect"

-- | Is a device connected?
isConnected :: Device -> BluetoothM Bool
isConnected dev = getDeviceProperty dev "Connected"

-- | Pair with a device
pairWith :: Device -> BluetoothM ()
pairWith dev = callDeviceMethod dev "Pair"

-- | Is device paired?
isPaired :: Device -> BluetoothM Bool
isPaired dev = getDeviceProperty dev "Paired"

-- | Trust device
trust :: Device -> BluetoothM ()
trust dev = setDeviceProperty dev "Trusted" True

-- | Stop trusting device
distrust :: Device -> BluetoothM ()
distrust dev = setDeviceProperty dev "Trusted" False

-- | Is device trusted?
isTrusted :: Device -> BluetoothM Bool
isTrusted dev = getDeviceProperty dev "Trusted"

getDeviceServiceUUIDs :: Device -> BluetoothM [UUID]
getDeviceServiceUUIDs dev = getDeviceProperty dev "UUIDs"


-- * Helpers

callDeviceMethod :: Device -> T.Text -> BluetoothM ()
callDeviceMethod dev methodName = do
  conn <- ask
  bluezName' <- liftIO $ readIORef bluezName
  toBluetoothM . const
    $ callMethod bluezName' (dev ^. path) (T.pack deviceIFace) methodName ()
      [] (dbusConn conn)

setDeviceProperty :: Representable a => Device -> T.Text -> a -> BluetoothM ()
setDeviceProperty dev propertyName newValue = do
  conn <- ask
  prop <- liftIO $ deviceRemoteProperty dev propertyName
  liftIO (setProperty prop newValue $ dbusConn conn) >>= \r -> case r of
    Left e -> throwError $ DBusError e
    Right () -> return ()

getDeviceProperty :: Representable a => Device -> T.Text -> BluetoothM a
getDeviceProperty dev propertyName = do
  conn <- ask
  prop <- liftIO $ deviceRemoteProperty dev propertyName
  liftIO (getProperty prop $ dbusConn conn) >>= \r -> case r of
    Left e -> throwError $ DBusError e
    Right v -> return v

deviceRemoteProperty :: Device -> T.Text -> IO (RemoteProperty (b :: DBusType))
deviceRemoteProperty dev propertyName = do
  bluezName' <- readIORef bluezName
  return $ RP
     { rpEntity = bluezName' -- Is this the right entity?
     , rpObject = dev ^. path
     , rpInterface = T.pack deviceIFace
     , rpName = propertyName
     }
