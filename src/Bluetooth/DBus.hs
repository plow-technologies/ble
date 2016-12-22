module Bluetooth.DBus where

import           Bluetooth.Utils
import           Bluetooth.Types
import           Bluetooth.HasInterface
import           Control.Monad.Reader
import qualified Data.Map             as Map
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

advertise :: WithObjectPath Advertisement -> BluetoothM ()
advertise adv = do
  conn <- ask
  liftIO $ addObject conn (adv ^. path) (adv `withInterface` leAdvertisementIFaceP)
  toBluetoothM . const
    $ callMethod bluezName bluezPath leAdvertisingManagerIFace "RegisterAdvertisement"  args []
    $ dbusConn conn
  where
    args :: (ObjectPath, Map.Map T.Text Any)
    args = (adv ^. path, Map.empty)

    
advertisementFor :: Application -> WithObjectPath Advertisement
advertisementFor app = WOP p adv
  where
    adv = def & serviceUUIDs .~ (app ^.. services . traversed . uuid)
    p = app ^. path & toText %~ (</> "adv")
    
-- * Constants

bluezName :: T.Text
bluezName = "org.bluez"

bluezPath :: ObjectPath
bluezPath = "/org/bluez/hci0"

gattManagerIFace :: T.Text
gattManagerIFace = "org.bluez.GattManager1"

gattServiceIFace :: T.Text
gattServiceIFace = "org.bluez.GattService1"

leAdvertisingManagerIFace :: T.Text
leAdvertisingManagerIFace = "org.bluez.LEAdvertisingManager1"
