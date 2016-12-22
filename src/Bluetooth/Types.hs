{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Bluetooth.Types where


import Bluetooth.Utils
import Control.Monad.Except   (ExceptT (ExceptT), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (MonadReader, ReaderT (ReaderT), runReaderT)
import Data.Default.Class     (Default (def))
import Data.IORef
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.String            (IsString (fromString))
import Data.Word              (Word16, Word32)
import DBus                   (ConnectionType (System), DBusConnection,
                               DBusSimpleType (..),
                               DBusType (DBusSimpleType, TypeDict, TypeVariant),
                               DBusValue (DBVDict, DBVVariant), MethodError,
                               MethodHandlerT, Object, ObjectPath,
                               Representable (..), connectBus, objectPath,
                               objectRoot)
import DBus.Types             (root)
import GHC.Generics           (Generic)
import Lens.Micro
import Lens.Micro.TH          (makeFields)
import Numeric                (readHex)

import qualified Data.ByteString as BS
import qualified Data.Map        as Map
import qualified Data.Text       as T
import qualified Data.UUID       as UUID
import qualified System.Random   as Rand


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

-- See <http://www.itu.int/rec/T-REC-X.667/en ITU-T Rec. X.677> for more
-- information on the format and generation of these UUIDs.
data UUID
  = OfficialUUID16 Word16
  | OfficialUUID32 Word32
  -- Custom services use 128-bit identifiers
  | UnofficialUUID UUID.UUID
  deriving (Eq, Show, Ord, Generic)

instance IsString UUID where
  fromString x
    | length x > 8  = UnofficialUUID
       $ fromMaybe (error "UUID.fromString: invalid UUID") $ UUID.fromString x
    | length x == 8 = OfficialUUID32 $ go x
    | length x == 4 = OfficialUUID16 $ go x
    | otherwise     = error "UUID.fromString: expecting 16, 32 or 128-bit UUID"
    where
    go y = case [ z | (z, "") <- readHex y ] of
      [val] -> val
      []    -> error "UUID.fromString: no parse"
      _     -> error "UUID.fromString: ambiguous parse"

instance Representable UUID where
  type RepType UUID = 'DBusSimpleType 'TypeString
  toRep (UnofficialUUID w) = toRep $ UUID.toText w
  fromRep x = do
    s <- fromRep x
    case T.length s of
      36 -> UnofficialUUID <$> UUID.fromText s
      _  -> Nothing

-- Random instance only generates unofficial UUIDs, since probably
-- that's the most common use-case. But this feels a little wrong.
instance Rand.Random UUID where
  randomR (UnofficialUUID lo, UnofficialUUID hi) g =
    let (a', g') = Rand.randomR (lo,hi) g in (UnofficialUUID a', g')
  randomR _ g = Rand.random g
  random g = let (a', g') = Rand.random g in (UnofficialUUID a', g')

-- * Any

-- | A Haskell existential type corresponding to DBus' @Variant@.
data Any where
  MkAny :: forall a . Representable a => a -> Any

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


-- * Descriptor

data Descriptor
  = ExtendedProperties
  | CharacteristicUserDescription
  | ClientCharacteristicConfiguration
  deriving (Eq, Show, Read, Generic, Ord)

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
  | CPWriteWithouResponse
  | CPWrite
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
  , (CPWriteWithouResponse, "write-without-response")
  , (CPWrite, "write")
  , (CPNotify, "notify")
  , (CPIndicate, "indicate")
  , (CPSignedWriteCommand, "authenticated-signed-writes")
  ]

data Characteristic = Characteristic
  { characteristicUuid       :: UUID
  , characteristicProperties :: [CharacteristicProperty]
  , characteristicReadValue  :: Maybe (MethodHandlerT IO BS.ByteString)
  , characteristicWriteValue :: Maybe (BS.ByteString -> MethodHandlerT IO BS.ByteString)
  } deriving (Generic)

makeFields ''Characteristic

characteristicAsDict :: ObjectPath -> Characteristic
  -> DBusValue ('TypeDict 'TypeString ('TypeDict 'TypeString 'TypeVariant))
characteristicAsDict opath char
  = toRep $ Map.fromList [(gattCharIFace, WOP opath char)]
    where
      gattCharIFace :: T.Text
      gattCharIFace = "org.bluez.GattCharacteristic1"

instance IsString Characteristic where
  fromString x = Characteristic (fromString x) [] Nothing Nothing

-- Note [WithObjectPath]
instance Representable (WithObjectPath Characteristic) where
  type RepType (WithObjectPath Characteristic)
    = 'TypeDict 'TypeString 'TypeVariant
  toRep char = toRep tmap
    where
      tmap :: Map.Map T.Text Any
      tmap = Map.fromList [ ("UUID", MkAny $ char ^. value . uuid)
                          , ("Service", MkAny $ char ^. path)
                          , ("Flags", MkAny $ char ^. value . properties)
                          ]
characteristicObjectPath :: ObjectPath -> Int -> ObjectPath
characteristicObjectPath appOPath idx = appOPath & toText %~ addSuffix
  where
    addSuffix r = r </> ("char" <> T.pack (show idx))

-- * Service

data Service = Service
  { serviceUuid            :: UUID
  , serviceCharacteristics :: [Characteristic]
  } deriving (Generic)

makeFields ''Service

instance IsString Service where
  fromString x = Service (fromString x) []

-- Note [WithObjectPath]
instance Representable (WithObjectPath Service) where
  type RepType (WithObjectPath Service) = 'TypeDict 'TypeString 'TypeVariant
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
        = (\x -> objectPath $ (serv ^. path . toText) </> ("char" <> T.pack (show x))) <$> [0..i]



-- * Application

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
                ('TypeDict 'TypeString ('TypeDict 'TypeString 'TypeVariant))
  toRep app = DBVDict $ zipWith servPaths ([0..]::[Int]) $ applicationServices app
    where
      servPaths i s = (toRep path', serviceAsDict s)
        where
          path' = serviceObjectPath (app ^. path) i
          gattServiceIFace :: T.Text
          gattServiceIFace = "org.bluez.GattService1"
          serviceAsDict serv
            = toRep $ Map.fromList [(gattServiceIFace, WOP path' serv)]
  fromRep _ = error "not implemented"


serviceObjectPath :: ObjectPath -> Int -> ObjectPath
serviceObjectPath appOPath idx = appOPath & toText %~ addSuffix
  where
    addSuffix r = r </> ("serv" <> T.pack (show idx))

-- * Advertisement

data AdvertisementType = Broadcast | Peripheral
  deriving (Eq, Show, Read, Generic, Bounded, Enum)

instance Representable AdvertisementType where
  type RepType AdvertisementType = 'DBusSimpleType 'TypeString
  toRep x = case x of
    Broadcast -> toRep ("broadcast" :: T.Text)
    Peripheral -> toRep ("peripheral" :: T.Text)

data Advertisement = Advertisement
  { advertisementType_            :: AdvertisementType
  , advertisementServiceUUIDs     :: [UUID]
  , advertisementSolicitUUIDs     :: [UUID]
  , advertisementManufacturerData :: Map.Map T.Text T.Text
  , advertisementServiceData      :: Map.Map UUID T.Text
  , advertisementIncludeTxPower   :: Bool
  } deriving (Eq, Show, Generic)

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
        , ("ManufacturerData", MkAny $ adv ^. manufacturerData)
        , ("ServiceData", MkAny $ adv ^. serviceData)
        , ("IncludeTxPower", MkAny $ adv ^. includeTxPower)
        ]

instance Default Advertisement where
  def = Advertisement Peripheral [] [] mempty mempty False



-- * Connection

-- The constructor should not be exported.
data Connection = Connection
  { dbusConn  :: DBusConnection
  -- Should it be possible to remove objects?
  , addObject :: ObjectPath -> Object -> IO ()
  }

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
