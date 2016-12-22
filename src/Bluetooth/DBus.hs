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
    args = (app ^. path, Map.empty)

-- | Adds handlers for all the objects managed by the Application (plus the
-- Application itself).
addAllObjs :: Connection -> Application -> BluetoothM ()
addAllObjs conn app = do
  liftIO $ addObject conn (app ^. path) (app `withInterface` objectManagerIFaceP)
  liftIO $ forM_ (zip [0..] (app ^. services)) $ \(i,s) -> do
    let p = serviceObjectPath (app ^. path) i 
    addObject conn p (WOP p s `withInterface` gattServiceIFaceP)
    forM_ (zip [0..] (s ^. characteristics)) $ \(i', c) -> do
      let p' = characteristicObjectPath p i'
      addObject conn p' (WOP p' c `withInterface` gattCharacteristicIFaceP)
    

-- * Constants

bluezName :: T.Text
bluezName = "org.bluez"

bluezPath :: ObjectPath
bluezPath = "/org/bluez/hci0"

gattManagerIFace :: T.Text
gattManagerIFace = "org.bluez.GattManager1"

gattServiceIFace :: T.Text
gattServiceIFace = "org.bluez.GattService1"
