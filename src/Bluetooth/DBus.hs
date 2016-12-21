module Bluetooth.DBus where

import           Bluetooth.Types
import           Bluetooth.HasInterface
import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Data.Proxy     
import qualified Data.Text            as T
import           DBus


-- | Registers an application (set of services) with Bluez.
registerApplication :: Application -> BluetoothM ()
registerApplication app = do
  conn <- asks dbusConn
  addObj <- asks addObject
  -- The top-level application needs to present an ObjectManager interface
  liftIO $ addObj (applicationRoot app) (app `withInterface` objectManagerIFace)
  toBluetoothM . const
    $ callMethod bluezName bluezPath gattManagerIFace "RegisterApplication"  args [] conn
  where
    args :: (ObjectPath, Map.Map T.Text Any)
    args = (applicationRoot app, Map.empty)


-- * Constants

bluezName :: T.Text
bluezName = "org.bluez"

bluezPath :: ObjectPath
bluezPath = "/org/bluez/hci0"

gattManagerIFace :: T.Text
gattManagerIFace = "org.bluez.GattManager1"

gattServiceIFace :: T.Text
gattServiceIFace = "org.bluez.GattService1"
