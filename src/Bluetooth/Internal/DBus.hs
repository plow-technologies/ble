module Bluetooth.Internal.DBus where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef           (readIORef, writeIORef)
import Data.Monoid          ((<>))
import DBus
import DBus.Signal          (execSignalT)
import Lens.Micro

import qualified Data.Map        as Map
import qualified Data.Text       as T

import Bluetooth.Internal.HasInterface
import Bluetooth.Internal.Interfaces
import Bluetooth.Internal.Types
import Bluetooth.Internal.Utils
import Bluetooth.Internal.Errors
import Bluetooth.Internal.Lenses

-- | Registers an application and advertises it. If you would like to have
-- finer-grained control of the advertisement, use @registerApplication@ and
-- @advertise@.
registerAndAdvertiseApplication :: Application -> BluetoothM ApplicationRegistered
registerAndAdvertiseApplication app = do
  reg <- registerApplication app
  advertise (advertisementFor app)
  return reg

-- | Registers an application (set of services) with Bluez.
registerApplication :: Application -> BluetoothM ApplicationRegistered
registerApplication app = do
  conn <- ask
  addAllObjs conn app
  () <- toBluetoothM . const
    $ callMethod bluezName bluezPath (T.pack gattManagerIFace)
        "RegisterApplication" args []
    $ dbusConn conn
  return $ ApplicationRegistered (app ^. path)
  where
    args :: (ObjectPath, Map.Map T.Text Any)
    args = (app ^. path, Map.empty)

unregisterApplication :: ApplicationRegistered -> BluetoothM ()
unregisterApplication (ApplicationRegistered appPath) = do
  conn <- ask
  toBluetoothM . const
    $ callMethod bluezName bluezPath (T.pack gattManagerIFace)
        "UnregisterApplication" appPath []
    $ dbusConn conn


-- | Adds handlers for all the objects managed by the Application (plus the
-- Application itself).
addAllObjs :: Connection -> Application -> BluetoothM ()
addAllObjs conn app = do
  liftIO $ addObject conn (app ^. path) (app `withInterface` objectManagerIFaceP)
  liftIO $ forM_ (zip [0..] (app ^. services)) $ \(i,s) -> do
    let p = serviceObjectPath (app ^. path) i
    addObject conn p
      $  (WOP p s `withInterface` gattServiceIFaceP)
      <> (WOP p s `withInterface` propertiesIFaceP)
    registerObjectPath (s ^. uuid) p
    forM_ (zip [0..] (s ^. characteristics)) $ \(i', c) -> do
      let p' = characteristicObjectPath p i'
      addObject conn p'
        $ (WOP p' c `withInterface` gattCharacteristicIFaceP)
       <> (WOP p' c `withInterface` propertiesIFaceP)
      registerObjectPath (c ^. uuid) p'
   where
     registerObjectPath :: UUID -> ObjectPath -> IO ()
     registerObjectPath uuid' op = writeIORef (objectPathOf uuid') (Just op)

-- | Advertise a set of services.
advertise :: WithObjectPath Advertisement -> BluetoothM ()
advertise adv = do
  conn <- ask
  liftIO $ do
    addObject conn (adv ^. path)
      $  (adv `withInterface` leAdvertisementIFaceP)
      <> ((adv ^. value) `withInterface` propertiesIFaceP)
  toBluetoothM . const $ do
    callMethod bluezName bluezPath (T.pack leAdvertisingManagerIFace) "RegisterAdvertisement" args []
      $ dbusConn conn
  where
    args :: (ObjectPath, Map.Map T.Text Any)
    args = (adv ^. path, Map.empty)

-- | Unregister an adverstisement.
unadvertise :: WithObjectPath Advertisement -> BluetoothM ()
unadvertise adv = do
  conn <- ask
  toBluetoothM . const $ do
    callMethod bluezName bluezPath (T.pack leAdvertisingManagerIFace) "UnregisterAdvertisement" args []
      $ dbusConn conn
  where
    args :: ObjectPath
    args = adv ^. path


-- | Create an advertisement for all of an application's services.
-- The advertisement will be for peripheral (not broadcast) by default.
advertisementFor :: Application -> WithObjectPath Advertisement
advertisementFor app = WOP p adv
  where
    adv = def & serviceUUIDs .~ (app ^.. services . traversed . uuid)
    p = app ^. path & toText %~ (</> "adv")


-- | Triggers notifications or indications.
triggerNotification :: ApplicationRegistered -> CharacteristicBS -> BluetoothM ()
triggerNotification (ApplicationRegistered _) c = do
   case c ^. readValue of
     Nothing -> throwError "Handler does not have a readValue implementation!"
     Just readHandler -> do
       res' <- liftIO $ runHandler readHandler
       res <- case res' of
         Left e -> throwError $ BLEError e
         Right v -> return v
       mPath <- liftIO $ readIORef $ objectPathOf (c ^. uuid)
       case mPath of
         Nothing -> throwError "UUID not found - are you sure you registered the application containing it?"
         Just path' -> runEff $ propertyChanged (valProp $ WOP path' c) res
  where
    runEff :: MethodHandlerT IO x -> BluetoothM x
    runEff act = do
      conn <- asks dbusConn
      res <- liftIO $ execSignalT act conn
      case res of
        Left e -> throwError . DBusError . MethodErrorMessage $ errorBody e
        Right val -> return val

-- | Get a service by UUID. Returns Nothing if the service could not be found.
getService :: UUID -> BluetoothM (Maybe Service)
getService serviceUUID = do
  services' <- getAllServices
  return $ case filter (\x -> x ^. uuid == serviceUUID) services' of
    [] -> Nothing
    -- This should never be a list with more than one element.
    (x:_) -> Just x

{--- | Get all registered services.-}
getAllServices :: BluetoothM [Service]
getAllServices = do
  conn <- ask
  objects :: [(ObjectPath, [(T.Text, Any)])]
    <- toBluetoothM . const $ callMethod bluezName "/" (T.pack objectManagerIFace)
       "GetManagedObjects" () [] $ dbusConn conn

  -- We need to construct services manually, since we get the characteristics
  -- separately.
  let chars =
       [ objPath
       | (objPath, ifaces) <- objects
       , T.pack gattCharacteristicIFace `elem` (fst <$> ifaces)
       ]
  let servs =
       [ objPath
       | (objPath, ifaces) <- objects
       , T.pack gattServiceIFace `elem` (fst <$> ifaces)
       ]
  forM servs $ \s -> mkService s [ c | c <- chars , s  `isPathPrefix` c ]
  where
    mkChar :: ObjectPath -> BluetoothM CharacteristicBS
    mkChar charPath = do
      conn <- ask
      charProps <- toBluetoothM . const $
        callMethod bluezName charPath (T.pack gattCharacteristicIFace)
          "GetAll" () [] $ dbusConn conn
      case charFromRep charProps of
        Nothing -> throwError $ OtherError
          "Bluez returned invalid characteristic"
        Just c -> return c

    mkService :: ObjectPath -> [ObjectPath] -> BluetoothM Service
    mkService servicePath charPaths = do
      conn <- ask
      serviceProps :: Map.Map T.Text UUID <- toBluetoothM . const $
        callMethod bluezName servicePath (T.pack gattServiceIFace)
          "GetAll" () [] $ dbusConn conn
      chars <- mapM mkChar charPaths
      case Map.lookup "UUID" serviceProps of
        Nothing -> throwError $ OtherError
          "Bluez did not return a UUID for service"
        Just u -> return $ Service u chars


  {-let processService servicePath = toBluetoothM . const $ do-}
        {-callMethod bluezName servicePath (T.pack gattServiceIFace)-}
          {-"GetAll" () [] $ dbusConn conn-}



-- * Constants

bluezName :: T.Text
bluezName = "org.bluez"

bluezPath :: ObjectPath
bluezPath = "/org/bluez/hci0"
