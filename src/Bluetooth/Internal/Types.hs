{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
#if !MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
module Bluetooth.Internal.Types where


import Control.Monad.Except   (ExceptT (ExceptT), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (MonadReader, ReaderT (ReaderT), runReaderT)
import Data.Default.Class     (Default (def))
import Data.IORef
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.String            (IsString (fromString))
import Data.Word              (Word16)
import DBus                   (ConnectionType (System), DBusConnection,
                               DBusSimpleType (..),
                               DBusType (DBusSimpleType, TypeDict, TypeVariant),
                               DBusValue (..), MethodError, Object, ObjectPath,
                               Representable (..), connectBus, objectPath,
                               objectRoot)
import DBus.Types             (dBusConnectionName, root)
import GHC.Generics           (Generic)
import Lens.Micro
import Lens.Micro.TH          (makeFields)

import qualified Data.ByteString as BS
import qualified Data.Map        as Map
import qualified Data.Text       as T
import qualified Data.UUID       as UUID
import qualified System.Random   as Rand

import Bluetooth.Internal.Errors
import Bluetooth.Internal.Interfaces
import Bluetooth.Internal.Utils

-- | Append two Texts, keeping exactly one slash between them.
(</>) :: T.Text -> T.Text -> T.Text
a </> b
  | "/" `T.isSuffixOf` a && "/" `T.isPrefixOf` b = a <> T.tail b
  | "/" `T.isSuffixOf` a || "/" `T.isPrefixOf` b = a <> b
  | otherwise                                    = a <> "/" <> b

parentPath :: T.Text -> T.Text
parentPath p = case reverse $ T.splitOn "/" p of
  _:xs -> T.intercalate "/" $ reverse xs
  []   -> "/"


-- * UUID

-- | UUIDs, used for services and characteristics.
--
-- Unofficial UUIDs will have 128-bits, and will look this:
--
--    d45e83fb-c772-459e-91a8-43cbf1443af4
--
-- Official UUIDs will have either 32 or 16 bits.
--
-- See <http://www.itu.int/rec/T-REC-X.667/en ITU-T Rec. X.677> for more
-- information on the format and generation of these UUIDs. You can use
-- the <https://www.uuidgenerator.net/ Online UUID Generator> to generate
-- UUIDs.
data UUID
  = UUID UUID.UUID
  deriving (Eq, Show, Ord, Generic)

baseUUID :: String
baseUUID = "-0000-1000-8000-00805F9B34FB"

instance IsString UUID where
  fromString x
    | length x > 8  = UUID
       $ fromMaybe (error "UUID.fromString: invalid UUID") $ UUID.fromString x
    | length x == 8 = UUID
       $ fromMaybe (error "UUID.fromString: invalid UUID") $ UUID.fromString
       $ x <> baseUUID
    | length x == 4 = UUID
       $ fromMaybe (error "UUID.fromString: invalid UUID") $ UUID.fromString
       $ "0000" <> x <> baseUUID
    | otherwise     = error "UUID.fromString: expecting 16, 32 or 128-bit UUID"


instance Representable UUID where
  type RepType UUID = 'DBusSimpleType 'TypeString
  toRep (UUID w) = toRep $ UUID.toText w
  fromRep x = do
    s <- fromRep x
    case T.length s of
      36 -> UUID <$> UUID.fromText s
      _  -> Nothing

instance Rand.Random UUID where
  randomR (UUID lo, UUID hi) g =
    let (a', g') = Rand.randomR (lo,hi) g in (UUID a', g')
  random g = let (a', g') = Rand.random g in (UUID a', g')

-- * Any

-- | A Haskell existential type corresponding to DBus' @Variant@.
data Any where
  MkAny :: forall a . (Representable a) => a -> Any

instance Representable Any where
  type RepType Any = 'TypeVariant
  toRep (MkAny x) = DBVVariant (toRep x)
  fromRep (DBVVariant x) = Just (MkAny x)

-- Note [WithObjectPath]
data WithObjectPath a = WOP
  { withObjectPathPath  :: ObjectPath
  , withObjectPathValue :: a
  } deriving (Eq, Show, Generic, Functor)

makeFields ''WithObjectPath

type AnyDBusDict = 'TypeDict 'TypeString 'TypeVariant

-- * Method

{-data Method where-}
  {-ReadValue :: ReadValueM BS.ByteString-}
  {-WriteValue :: BS.ByteString -> WriteValueM BS.ByteString-}
  {-Notify :: -}

-- * Descriptor

data Descriptor = Descriptor
  { descriptorUuid :: UUID
  } deriving (Eq, Show, Generic)


data AdvertisingPacketType
  = ConnectableUndirected
  | ConnectableDirected
  | NonConnnectableUndirected
  | ScannableUndirected
  deriving (Eq, Show, Read, Generic, Ord)

-- * Characteristic

data CharacteristicProperty
  = CPBroadcast
  | CPRead
  | CPEncryptRead
  | CPEncryptAuthenticatedRead
  | CPWriteWithoutResponse
  | CPWrite
  | CPEncryptWrite
  | CPEncryptAuthenticatedWrite
  | CPAuthenticatedSignedWrites
  | CPNotify
  | CPIndicate
  | CPSignedWriteCommand
  deriving (Eq, Show, Read, Enum, Bounded, Ord, Generic)

instance Representable CharacteristicProperty where
  type RepType CharacteristicProperty = 'DBusSimpleType 'TypeString
  toRep x = maybe (error "impossible") toRep $ lookup x chrPropPairs
  fromRep x = do
    key <- fromRep x
    let swapped = (\(a,b) -> (b,a)) <$> chrPropPairs
    lookup key swapped


chrPropPairs :: [(CharacteristicProperty, T.Text)]
chrPropPairs =
  [ (CPBroadcast, "broadcast")
  , (CPRead, "read")
  , (CPEncryptRead, "encrypt-read")
  , (CPEncryptAuthenticatedRead, "encrypt-authenticated-read")
  , (CPWriteWithoutResponse, "write-without-response")
  , (CPWrite, "write")
  , (CPEncryptWrite, "encrypt-write")
  , (CPEncryptAuthenticatedWrite, "encrypt-authenticated-write")
  , (CPAuthenticatedSignedWrites, "authenticated-signed-writes")
  , (CPNotify, "notify")
  , (CPIndicate, "indicate")
  , (CPSignedWriteCommand, "authenticated-signed-writes")
  ]

data CharacteristicOptions = CharacteristicOptions
  { characteristicOptionsOffset :: Maybe Word16
  } deriving (Eq, Show, Read, Generic)

makeFields ''CharacteristicOptions

instance Representable CharacteristicOptions where
  type RepType CharacteristicOptions = AnyDBusDict
  fromRep x = do
    m <- fromRep x
    return $ case Map.lookup ("offset" :: T.Text) m of
      Just (DBVVariant (DBVUInt16 w)) -> CharacteristicOptions (Just w)
      _            -> CharacteristicOptions Nothing
  toRep x = case x ^. offset of
    Nothing -> DBVDict []
    Just v  -> DBVDict [(toRep ("offset" :: T.Text), toRep $ MkAny v)]

type CharacteristicBS = Characteristic BS.ByteString

data Characteristic typ = Characteristic
  { characteristicUuid       :: UUID
  , characteristicProperties :: [CharacteristicProperty]
  , characteristicReadValue  :: Maybe (Handler typ)
  -- | Write a value. Note that the value is only writeable externally if the
  -- characteristic contains the CPWrite property *and* this is a Just.
  , characteristicWriteValue :: Maybe (typ -> Handler Bool)
  -- | If @Nothing@, this characteristic does not send notifications.
  -- If @Just False@, the characteristic does not currently send notifications, but
  -- can be made to (with a @StartNotify@ method request).
  -- If @Just True@, the characteristic currently sends notifications (and can
  -- be made to stop with a @StopNotify@ method request).
  -- **NOTE**: Notifications do not currently work.
  , characteristicNotifying  :: Maybe (IORef Bool)
  } deriving (Generic)

makeFields ''Characteristic


instance IsString (Characteristic a) where
  fromString x = Characteristic (fromString x) [] Nothing Nothing Nothing

-- Note [WithObjectPath]
instance Representable (WithObjectPath (Characteristic a)) where
  type RepType (WithObjectPath (Characteristic a)) = AnyDBusDict
  toRep char = toRep tmap
    where
      tmap :: Map.Map T.Text Any
      tmap = Map.fromList [ ("UUID", MkAny $ char ^. value . uuid)
                          , ("Service", MkAny $ (char ^. path) & toText %~ parentPath)
                          , ("Flags", MkAny $ char ^. value . properties)
                          ]
  fromRep _ = error "not implemented"

characteristicObjectPath :: ObjectPath -> Int -> ObjectPath
characteristicObjectPath appOPath idx = appOPath & toText %~ addSuffix
  where
    fourDigits = T.pack $ case show idx of
      [a] -> ['0','0','0',a]
      [a,b] -> ['0','0',a,b]
      [a,b,c] -> ['0',a,b,c]
      [a,b,c,d] -> [a,b,c,d]
      _ -> error "maximum 9999 characteristics"
    addSuffix r = r </> ("char" <> fourDigits)

-- * Service

data Service = Service
  { serviceUuid            :: UUID
  , serviceCharacteristics :: [CharacteristicBS]
  } deriving (Generic)

makeFields ''Service

instance IsString Service where
  fromString x = Service (fromString x) []

-- Note [WithObjectPath]
instance Representable (WithObjectPath Service) where
  type RepType (WithObjectPath Service) = AnyDBusDict
  toRep serv = toRep tmap
    where
      tmap :: Map.Map T.Text Any
      tmap = Map.fromList
        [ ("UUID", MkAny $ serv ^. value . uuid )
        -- Only primary services for now
        , ("Primary", MkAny $ True)
        , ("Characteristics", MkAny (charPaths . length $ serv ^. value . characteristics))
        ]

      charPaths :: Int -> [ObjectPath]
      charPaths i
        = characteristicObjectPath (objectPath $ serv ^. path . toText) <$> [0..i-1]

  fromRep _ = error "not implemented"


-- * Application

-- | An application. Can be created from it's @IsString@ instance.
-- The string (application path) is used only for the DBus API, and will not
-- have relevance within Bluetooth.
data Application = Application
  { applicationPath     :: ObjectPath
  , applicationServices :: [Service]
  } deriving (Generic)

makeFields ''Application

instance IsString Application where
  fromString x = Application (fromString x) []

instance Representable Application where
  type RepType Application
    = 'TypeDict 'TypeObjectPath
                ('TypeDict 'TypeString AnyDBusDict)
  toRep app = toRep $ Map.fromList $ concat $ do
    (idxS, serv) <- zip [0..] (app ^. services)
    let servPath = serviceObjectPath (app ^. path) idxS
        chars = do
          (idxC, char) <- zip [0..] (serv ^. characteristics)
          let charPath = characteristicObjectPath servPath idxC
          return $ charAsEntry charPath char
    return $ serviceAsEntry servPath serv : chars
    where
     serviceAsEntry path' serv
       = (path', toRep $ Map.fromList [(T.pack gattServiceIFace, WOP path' serv)])
     charAsEntry path' char
       = (path', toRep $ Map.fromList [(T.pack gattCharacteristicIFace, WOP path' char)])
  fromRep _ = error "not implemented"


serviceObjectPath :: ObjectPath -> Int -> ObjectPath
serviceObjectPath appOPath idx = appOPath & toText %~ addSuffix
  where
    twoDigits = T.pack $ case show idx of
      [a] -> ['0', a]
      [a,b] -> [a, b]
      _ -> error "maximum 99 services"
    addSuffix r = r </> ("service" <> twoDigits)

-- * Advertisement

data AdvertisementType = Broadcast | Peripheral
  deriving (Eq, Show, Read, Generic, Bounded, Enum)

instance Representable AdvertisementType where
  type RepType AdvertisementType = 'DBusSimpleType 'TypeString
  toRep x = case x of
    Broadcast -> toRep ("broadcast" :: T.Text)
    Peripheral -> toRep ("peripheral" :: T.Text)
  fromRep _ = error "not implemented"

data Advertisement = Advertisement
  { advertisementType_            :: AdvertisementType
  , advertisementServiceUUIDs     :: [UUID]
  , advertisementSolicitUUIDs     :: [UUID]
  , advertisementManufacturerData :: Map.Map Word16 BS.ByteString
  , advertisementServiceData      :: Map.Map UUID BS.ByteString
  , advertisementIncludeTxPower   :: Bool
  } deriving (Generic)

makeFields ''Advertisement

instance Representable Advertisement where
  type RepType Advertisement = 'TypeDict 'TypeString 'TypeVariant
  toRep adv = toRep m
    where
      m :: Map.Map T.Text Any
      m = Map.fromList
        [ ("Type", MkAny $ adv ^. type_)
        , ("ServiceUUIDs", MkAny $ adv ^. serviceUUIDs)
        , ("SolicitUUIDs", MkAny $ adv ^. solicitUUIDs)
#ifdef BluezGEQ543
        , ("ManufacturerData", MkAny $ MkAny <$> adv ^. manufacturerData)
        , ("ServiceData", MkAny $ MkAny <$> adv ^. serviceData)
#else
        , ("ManufacturerData", MkAny $ adv ^. manufacturerData)
        , ("ServiceData", MkAny $ adv ^. serviceData)
#endif
        , ("IncludeTxPower", MkAny $ adv ^. includeTxPower)
        ]
  fromRep _ = error "not implemented"

instance Default Advertisement where
  def = Advertisement Peripheral [] [] mempty mempty False



-- * Connection

-- The constructor should not be exported.
data Connection = Connection
  { dbusConn  :: DBusConnection
  -- Should it be possible to remove objects?
  , addObject :: ObjectPath -> Object -> IO ()
  }

-- | The unique DBus connection name, Useful for monitoring activity with
-- 'dbus-monitor'. For information on how to setup dbus-monitor for debugging,
-- see <https://wiki.ubuntu.com/DebuggingDBus DebuggingDBus>.
--
-- @since 0.1.3.0
connectionName :: Connection -> T.Text
connectionName = dBusConnectionName . dbusConn


-- | Creates a connection to DBus. This does *not* represent Bluetooth
-- connection.
connect :: IO Connection
connect = do
  let noHandler _ _ _ = return ()
  ref <- newIORef mempty
  let addObj objPath obj = modifyIORef' ref (root objPath obj `mappend`)
      methodHandler conn hdr val = readIORef ref >>= \f -> objectRoot f conn hdr val
  dbusC <- connectBus System methodHandler noHandler
  return $ Connection dbusC addObj

-- * BluetoothM

newtype BluetoothM a
  = BluetoothM ( ReaderT Connection (ExceptT MethodError IO) a )
  deriving (Functor, Applicative, Monad, MonadIO, MonadError MethodError,
            MonadReader Connection)

runBluetoothM :: BluetoothM a -> Connection -> IO (Either MethodError a)
runBluetoothM (BluetoothM e) conn = runExceptT $ runReaderT e conn

toBluetoothM :: (Connection -> IO (Either MethodError a)) -> BluetoothM a
toBluetoothM = BluetoothM . ReaderT . fmap ExceptT



{- Note [WithObjectPath]
~~~~~~~~~~~~~~~~~~~~~~~~~

In OOP, which is explicitly the programming model around which
DBus was designed, methods have access to the object they are
a method of. Here, we prefer to not unnecessarily tie services,
characteristics, and descriptors to the data that may have them
as fields. This makes it possible for different services to have
the "same" characteristic (e.g.).

But the ObjectPath of each of these types messes up with this,
since it depends on the 'object' of which this characteristic
is a property or method.

So we use WithObjectPath to attach ObjectPaths to these values,
and write instances for Representable for @WithObjectPath a@
rather than @a@.
-}
