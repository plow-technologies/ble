module Bluetooth.DBus where

import Data.Monoid
import Bluetooth.Types
import qualified Data.Text as T
import DBus

registerService :: Service -> BluetoothM ()
registerService service = toBluetoothM $
  callMethod bluezName bluezPath gattManagerIFace "RegisterService" opath [] 
  where
    opath = objectPath $ "org/bluez/external/" <> serviceName service

connect :: IO DBusConnection
connect = connectClient System

-- * Constants

bluezName :: T.Text
bluezName = "org.bluez"

bluezPath :: ObjectPath
bluezPath = "org.bluez"

gattManagerIFace :: T.Text
gattManagerIFace = "org.bluez.GattManager1"
