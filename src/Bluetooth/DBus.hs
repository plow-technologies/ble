module Bluetooth.DBus where

import           Bluetooth.Types
import           Bluetooth.HasInterface
import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Data.Proxy     
import qualified Data.Text            as T
import           DBus
import           Lens.Micro


-- | Registers an application (set of services) with Bluez.
registerApplication :: Application -> BluetoothM ()
registerApplication app = do
  conn <- ask
  addAllObjs conn app
  toBluetoothM . const
    $ callMethod bluezName bluezPath gattManagerIFace "RegisterApplication"  args []
    $ dbusConn conn
  where
    args :: (ObjectPath, Map.Map T.Text Any)
    args = (app ^. root, Map.empty)

-- | Adds handlers for all the objects managed by the Application (plus the
-- Application itself).
addAllObjs :: Connection -> Application -> BluetoothM ()
addAllObjs conn app = do
  liftIO $ addObject conn (app ^. root) (app `withInterface` objectManagerIFaceP)
  liftIO $ forM_ (app ^. services) $ \s -> do
    addObject conn s (s `withInterface` gattServiceIFaceP)
    

-- * Constants

bluezName :: T.Text
bluezName = "org.bluez"

bluezPath :: ObjectPath
bluezPath = "/org/bluez/hci0"

gattManagerIFace :: T.Text
gattManagerIFace = "org.bluez.GattManager1"

gattServiceIFace :: T.Text
gattServiceIFace = "org.bluez.GattService1"
