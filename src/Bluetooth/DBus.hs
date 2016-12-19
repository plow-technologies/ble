module Bluetooth.DBus where

import Bluetooth.Types
import qualified Data.Text as T
import DBus

bluezName :: T.Text
bluezName = "org.bluez"

bluezPath :: ObjectPath
bluezPath = "org.bluez"

gattManagerIFace :: T.Text
gattManagerIFace = "org.bluez.GattManager1"

registerService :: Service -> BluetoothM ()
registerService service = toBluetoothM $
  callMethod bluezName bluezPath gattManagerIFace "RegisterService" () [] 
