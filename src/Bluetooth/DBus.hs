module Bluetooth.DBus where

import Bluetooth.Types
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Map as Map
import DBus
import DBus.Types (object)

-- | Registers an application (set of services) with Bluez.
registerApplication :: Application -> BluetoothM ()
registerApplication app = do
  conn <- asks dbusConn
  addObj <- asks addObject
  -- The top-level application needs to present an ObjectManager interface
  liftIO $ addObj (applicationRoot app) appObj
  toBluetoothM . const
    $ callMethod bluezName bluezPath gattManagerIFace "RegisterApplication"  args [] conn
  -- forM_ (applicationServices app) 
  where
    args :: (ObjectPath, Map.Map T.Text Any)
    args = (applicationRoot app, Map.empty)

    appObj :: Object
    appObj = object "org.freedesktop.DBus.ObjectManager"
                    objectManagerIFace

    -- See https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-objectmanager
    objectManagerIFace :: Interface
    objectManagerIFace = Interface
      { interfaceMethods = [getManagedObjects]
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = []
      }

    getManagedObjects :: Method
    getManagedObjects
      = Method (repMethod (return app :: IO Application))
               "GetManagedObjects"
               Done
               ("rep" :> Done)
 

-- * Constants

bluezName :: T.Text
bluezName = "org.bluez"

bluezPath :: ObjectPath
bluezPath = "/org/bluez/hci0"

gattManagerIFace :: T.Text
gattManagerIFace = "org.bluez.GattManager1"

gattServiceIFace :: T.Text
gattServiceIFace = "org.bluez.GattService1"
