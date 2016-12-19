module Bluetooth.DBus where

import Bluetooth.Types
import qualified Data.Text as T
import qualified Data.Map as Map
import DBus

registerApplication :: Application -> BluetoothM ()
registerApplication app = toBluetoothM $
  callMethod bluezName bluezPath gattManagerIFace "RegisterApplication" args []
  where
    args :: (ObjectPath, Map.Map T.Text Any)
    args = (applicationRoot app, Map.empty)
 
connect :: IO DBusConnection
connect = connectClient System

-- * Constants

bluezName :: T.Text
bluezName = "org.bluez"

bluezPath :: ObjectPath
bluezPath = "/org/bluez/hci0"

gattManagerIFace :: T.Text
gattManagerIFace = "org.bluez.GattManager1"
