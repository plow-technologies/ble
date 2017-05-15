module Bluetooth.Internal.DBus where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef           (readIORef, writeIORef)
import Data.Monoid          ((<>))
import DBus
import DBus.Signal          (execSignalT)
import DBus.Types           (RemoteProperty (..))
import Lens.Micro

import qualified Data.ByteString as BS
import qualified Data.Map        as Map
import qualified Data.Text       as T

import Bluetooth.Internal.Errors
import Bluetooth.Internal.HasInterface
import Bluetooth.Internal.Interfaces
import Bluetooth.Internal.Lenses
import Bluetooth.Internal.Types
import Bluetooth.Internal.Utils


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
  bluezPath' <- liftIO $ readIORef bluezPath
  () <- callMethodBM bluezPath' gattManagerIFace "RegisterApplication" args
  return $ ApplicationRegistered (app ^. path)
  where
    args :: (ObjectPath, Map.Map T.Text Any)
    args = (app ^. path, Map.empty)

unregisterApplication :: ApplicationRegistered -> BluetoothM ()
unregisterApplication (ApplicationRegistered appPath) = do
  bluezPath' <- liftIO $ readIORef bluezPath
  callMethodBM bluezPath' gattManagerIFace "UnregisterApplication" appPath


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
  bluezPath' <- liftIO $ readIORef bluezPath
  callMethodBM bluezPath' leAdvertisingManagerIFace "RegisterAdvertisement" args
  where
    args :: (ObjectPath, Map.Map T.Text Any)
    args = (adv ^. path, Map.empty)

-- | Unregister an adverstisement.
unadvertise :: WithObjectPath Advertisement -> BluetoothM ()
unadvertise adv = do
  bluezPath' <- liftIO $ readIORef bluezPath
  callMethodBM bluezPath' leAdvertisingManagerIFace "UnregisterAdvertisement" args
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
triggerNotification :: ApplicationRegistered -> CharacteristicBS 'Local -> BluetoothM ()
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
getService :: UUID -> BluetoothM (Maybe (Service 'Remote))
getService serviceUUID = do
  services' <- getAllServices
  return $ case filter (\x -> x ^. uuid == serviceUUID) services' of
    [] -> Nothing
    -- This should never be a list with more than one element.
    (x:_) -> Just x

-- | Get all registered services.
getAllServices :: BluetoothM [Service 'Remote]
getAllServices = do
  objects :: Map.Map ObjectPath (Map.Map T.Text (Map.Map T.Text DontCareFromRep))
    <- callMethodBM "/" objectManagerIFace "GetManagedObjects" ()

  -- We need to construct services manually, since we get the characteristics
  -- separately.
  let chars =
       Map.keys $ Map.filter (T.pack gattCharacteristicIFace `Map.member`) objects
  let servs =
       Map.keys $ Map.filter (T.pack gattServiceIFace `Map.member`) objects
  forM servs $ \s -> mkService s [ c | c <- chars , s  `isPathPrefix` c ]

  where

    readProps :: [CharacteristicProperty]
    readProps = [CPRead, CPEncryptRead, CPEncryptAuthenticatedRead]

    writeProps :: [CharacteristicProperty]
    writeProps = [ CPWrite, CPWriteWithoutResponse, CPEncryptWrite
                 , CPEncryptAuthenticatedRead, CPAuthenticatedSignedWrites]

    charFromRep :: ObjectPath -> DBusValue AnyDBusDict
      -> Maybe (CharacteristicBS 'Remote)
    charFromRep charPath dict' = do
      dict :: Map.Map T.Text (DBusValue 'TypeVariant) <- fromRep dict'
      let unmakeAny :: (Representable a) => DBusValue 'TypeVariant -> Maybe a
          unmakeAny x = fromRep =<< fromVariant x
      uuid' :: UUID <- unmakeAny =<< Map.lookup "UUID" dict
      properties' <- unmakeAny =<< Map.lookup "Flags" dict
      let mrv = if any (`elem` readProps) properties'
            then Just $
              callMethodBM charPath gattCharacteristicIFace "ReadValue" ()
            else Nothing
      let mwv = if any (`elem` writeProps) properties'
            then Just $
              callMethodBM charPath gattCharacteristicIFace "WriteValue"
            else Nothing
      let char = RemoteChar uuid' properties' mrv mwv charPath
      return char

    mkChar :: ObjectPath -> BluetoothM (CharacteristicBS 'Remote)
    mkChar charPath = do
      charProps  <- callMethodBM charPath propertiesIFace "GetAll"
        (T.pack gattCharacteristicIFace)
      case charFromRep charPath charProps of
        Nothing -> throwError $ OtherError
          "Bluez returned invalid characteristic"
        Just c -> return c

    serviceFromRep :: DBusValue AnyDBusDict -> Maybe (Service m)
    serviceFromRep dict' = do
      dict :: Map.Map T.Text (DBusValue 'TypeVariant) <- fromRep dict'
      let unmakeAny :: (Representable a) => DBusValue 'TypeVariant -> Maybe a
          unmakeAny x = fromRep =<< fromVariant x
      uuid' :: UUID <- unmakeAny =<< Map.lookup "UUID" dict
      return $ Service uuid' []

    mkService :: ObjectPath -> [ObjectPath] -> BluetoothM (Service 'Remote)
    mkService servicePath charPaths = do
      serviceProps <- callMethodBM servicePath propertiesIFace "GetAll"
        (T.pack gattServiceIFace)
      chars <- mapM mkChar charPaths
      case serviceFromRep serviceProps of
        Nothing -> throwError $ OtherError
          "Bluez returned invalid service"
        Just serv -> return $ serv & characteristics .~ chars

-- | Starts notifications on the remote (peripheral), handling it with the
-- provided callback.
startNotify ::
  {-(Representable a, ArgParity (FlattenRepType (RepType a)) ~ 'Arg 'Null)-}
  CharacteristicBS 'Remote -> (BS.ByteString -> IO ()) -> BluetoothM ()
startNotify char handler = do
  conn <- dbusConn <$> ask
  rprop <- liftIO $ valRemoteProp char
  liftIO $ handlePropertyChanged rprop (whenJust handler) conn
  callMethodBM (char ^. path) gattCharacteristicIFace "StartNotify" ()
  where
    whenJust fn (Just v) = fn v
    whenJust _  Nothing  = return ()

-- | Stop notifications on the remote (peripheral).
stopNotify :: Characteristic 'Remote a -> BluetoothM ()
stopNotify char
  = callMethodBM (char ^. path) gattCharacteristicIFace "StopNotify" ()

-- Helpers

valRemoteProp :: CharacteristicBS 'Remote -> IO (RemoteProperty (RepType BS.ByteString))
valRemoteProp char = do
  bluezName' <- readIORef bluezName
  return $ RP
     { rpEntity = bluezName' -- Is this the right entity?
     , rpObject = char ^. path
     , rpInterface = T.pack gattCharacteristicIFace
     , rpName = "Value"
     }

callMethodBM :: (Representable args, Representable ret)
  => ObjectPath
  -> String
  -> T.Text
  -> args
  -> BluetoothM ret
callMethodBM opath iface methodName args = do
  conn <- ask
  bluezName' <- liftIO $ readIORef bluezName
  toBluetoothM . const $ callMethod bluezName' opath (T.pack iface) methodName args [] (dbusConn conn)
