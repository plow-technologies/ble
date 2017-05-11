{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -ddump-splices   #-}
#if !MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
module Bluetooth.Internal.Types where


import Control.Concurrent     (MVar, modifyMVar, newMVar)
import Control.Monad.Except   (ExceptT (ExceptT), MonadError, runExceptT,
                               withExceptT)
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
import GHC.Exts               (IsList (..))
import GHC.Generics           (Generic)
import Lens.Micro
import System.IO.Unsafe       (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.Map        as Map
import qualified Data.Text       as T
import qualified Data.UUID       as UUID
import qualified System.Random   as Rand

import Bluetooth.Internal.Errors
import Bluetooth.Internal.Interfaces
import Bluetooth.Internal.Utils
import Bluetooth.Internal.Lenses


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

-- * Place

-- | Whether a service/characteristic is running on this computer (`Local`) or
-- another (`Remote`). If `Local`, we're acting as a peripheral; otherwise, as
-- a central.
data Location = Local | Remote
  deriving (Eq, Show, Read, Generic)

{-
-- This should actually be a closed type family, but because TemplateHaskell
-- insists on things being defined in a specific order, we make it open an
-- define instances later
type family HandlerType (loc :: Location) (v :: *) :: * where
    Handler
-}

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

-- * Any and DontCare

-- | A Haskell existential type corresponding to DBus' @Variant@.
data Any where
  MkAny :: forall a . (Representable a) => a -> Any

instance Representable Any where
  type RepType Any = 'TypeVariant
  toRep (MkAny x) = DBVVariant (toRep x)
  fromRep = error "not implemented"

-- Note [WithObjectPath]
data WithObjectPath a = WOP
  { withObjectPathPath  :: ObjectPath
  , withObjectPathValue :: a
  } deriving (Eq, Show, Generic, Functor)

instance HasPath (WithObjectPath a) ObjectPath where
  {-# INLINE path #-}
  path f (WOP p v)
    = fmap (\y -> WOP y v) (f p)
instance HasValue (WithObjectPath a) a where
  {-# INLINE value #-}
  value f (WOP p v)
    = fmap (\ y -> WOP p y) (f v)

data DontCareFromRep = DontCareFromRep
  deriving (Eq, Show, Read, Generic)

instance Representable DontCareFromRep where
  type RepType DontCareFromRep = 'TypeVariant
  toRep _ = DBVVariant (toRep ())
  fromRep _ = Just DontCareFromRep

type AnyDBusDict = 'TypeDict 'TypeString 'TypeVariant

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
  ]

data CharacteristicOptions = CharacteristicOptions
  { characteristicOptionsOffset :: Maybe Word16
  } deriving (Eq, Show, Read, Generic)

instance HasOffset CharacteristicOptions (Maybe Word16) where
  {-# INLINE offset #-}
  offset f (CharacteristicOptions x)
    = fmap (\ y -> CharacteristicOptions y) (f x)

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

type CharacteristicBS m = Characteristic m BS.ByteString

data Characteristic h typ = Characteristic
  { characteristicUuid       :: UUID
  , characteristicProperties :: [CharacteristicProperty]
  , characteristicReadValue  :: Maybe (h typ)
  -- | Write a value. Note that the value is only writeable externally if the
  -- characteristic contains the CPWrite property *and* this is a Just.
  , characteristicWriteValue :: Maybe (typ -> (h Bool))
  } deriving (Generic)

instance HasProperties (Characteristic m v) [CharacteristicProperty] where
  {-# INLINE properties #-}
  properties f (Characteristic u p rv wv)
    = fmap (\ y -> Characteristic u y rv wv) (f p)

instance HasReadValue (Characteristic m v) (Maybe (m v)) where
  {-# INLINE readValue #-}
  readValue f (Characteristic u p rv wv)
    = fmap (\ y -> Characteristic u p y wv) (f rv)

instance HasUuid (Characteristic m v) UUID where
  {-# INLINE uuid #-}
  uuid f (Characteristic u p rv wv)
    = fmap (\ y -> Characteristic y p rv wv) (f u)

instance HasWriteValue (Characteristic m v) (Maybe (v -> m Bool)) where
  {-# INLINE writeValue #-}
  writeValue f (Characteristic u p rv wv)
    = fmap (\ y -> Characteristic u p rv y) (f wv)


-- This is essentialy the unsafePerformIO memoization trick
characteristicIsNotifying :: UUID -> MVar Bool
characteristicIsNotifying = unsafePerformIO $ do
  cm <- newMVar $ Map.empty
  return $ \uuid' -> unsafePerformIO $ do
    modifyMVar cm $ \curMap -> case Map.lookup uuid' curMap of
      Nothing -> do
        e <- newMVar False
        return (Map.insert uuid' e curMap, e)
      Just v  -> return (curMap, v)
{-# NOINLINE characteristicIsNotifying #-}

-- This too is essentialy the unsafePerformIO memoization trick. Keeps track of
-- object paths for registered services and characteristics so that we can
-- expose an API that doesn't require WithObjectPath
objectPathOf :: UUID -> IORef (Maybe ObjectPath)
objectPathOf = unsafePerformIO $ do
  cm <- newMVar $ Map.empty
  return $ \uuid' -> unsafePerformIO $ do
    modifyMVar cm $ \curMap -> case Map.lookup uuid' curMap of
      Nothing -> do
        e <- newIORef Nothing
        return (Map.insert uuid' e curMap, e)
      Just v  -> return (curMap, v)
{-# NOINLINE objectPathOf #-}

instance IsString (Characteristic m a) where
  fromString x = Characteristic (fromString x) [] Nothing Nothing

-- Note [WithObjectPath]
instance Representable (WithObjectPath (Characteristic m a)) where
  type RepType (WithObjectPath (Characteristic m a)) = AnyDBusDict
  toRep char = toRep tmap
    where
      tmap :: Map.Map T.Text Any
      tmap = Map.fromList [ ("UUID", MkAny $ char ^. value . uuid)
                          , ("Service", MkAny $ (char ^. path) & toText %~ parentPath)
                          , ("Flags", MkAny $ char ^. value . properties)
                          ]
  fromRep = error "not implemented"


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

data Service m = Service
  { serviceUuid            :: UUID
  , serviceCharacteristics :: [CharacteristicBS m]
  } deriving (Generic)

instance HasCharacteristics (Service m) [CharacteristicBS m] where
  {-# INLINE characteristics #-}
  characteristics f (Service u c)
    = fmap (\ y -> Service u y) (f c)

instance HasUuid (Service m) UUID where
  {-# INLINE uuid #-}
  uuid f (Service u c)
    = fmap (\ y -> Service y c) (f u)

instance IsString (Service m) where
  fromString x = Service (fromString x) []

-- Note [WithObjectPath]
instance Representable (WithObjectPath (Service m)) where
  type RepType (WithObjectPath (Service m)) = AnyDBusDict
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
  , applicationServices :: [Service Handler]
  } deriving (Generic)


instance HasPath Application ObjectPath where
  {-# INLINE path #-}
  path f (Application p s)
    = fmap (\ y -> Application y s) (f p)

instance HasServices Application [Service Handler] where
  {-# INLINE services #-}
  services f (Application p s)
    = fmap (\ y -> Application p y) (f s)

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

-- | An advertisement can be generated automatically with @advertisementFor@,
-- or with the @IsList@ instance. Both of these by default assume the
-- advertisement is for a peripheral.
--
-- You can also, of course, use the constructor.
data Advertisement = Advertisement
  { advertisementType_            :: AdvertisementType
  , advertisementServiceUUIDs     :: [UUID]
  , advertisementSolicitUUIDs     :: [UUID]
  , advertisementManufacturerData :: Map.Map Word16 BS.ByteString
  , advertisementServiceData      :: Map.Map UUID BS.ByteString
  , advertisementIncludeTxPower   :: Bool
  } deriving (Eq, Show, Generic)

instance HasIncludeTxPower Advertisement Bool where
  {-# INLINE includeTxPower #-}
  includeTxPower
    fn
    (Advertisement a b c d e f)
    = fmap (\ y -> Advertisement a b c d e y) (fn f)

instance HasManufacturerData Advertisement (Map.Map Word16 BS.ByteString) where
  {-# INLINE manufacturerData #-}
  manufacturerData
    fn
    (Advertisement a b c d e f)
    = fmap (\ y -> Advertisement a b c y e f) (fn d)

instance HasServiceData Advertisement (Map.Map UUID BS.ByteString) where
  {-# INLINE serviceData #-}
  serviceData
    fn
    (Advertisement a b c d e f)
    = fmap (\ y -> Advertisement a b c d y f) (fn e)

instance HasServiceUUIDs Advertisement [UUID] where
  {-# INLINE serviceUUIDs #-}
  serviceUUIDs
    fn
    (Advertisement a b c d e f)
    = fmap (\ y -> Advertisement a y c d e f) (fn b)

instance HasSolicitUUIDs Advertisement [UUID] where
  {-# INLINE solicitUUIDs #-}
  solicitUUIDs
    fn
    (Advertisement a b c d e f)
    = fmap
        (\ y -> Advertisement a b y d e f) (fn c)

instance HasType_ Advertisement AdvertisementType where
  {-# INLINE type_ #-}
  type_
    fn
    (Advertisement a b c d e f)
    = fmap (\ y -> Advertisement y b c d e f) (fn a)

instance IsList Advertisement where
  type Item Advertisement = UUID
  fromList services' = Advertisement
    { advertisementType_            = Peripheral
    , advertisementServiceUUIDs     = services'
    , advertisementSolicitUUIDs     = []
    , advertisementManufacturerData = mempty
    , advertisementServiceData      = mempty
    , advertisementIncludeTxPower   = False
    }
  toList adv = adv ^. serviceUUIDs

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

data Error
  = DBusError MethodError
  | BLEError T.Text
  | OtherError T.Text
  deriving (Show, Generic)

instance IsString Error where
  fromString = BLEError . fromString

newtype BluetoothM a
  = BluetoothM ( ReaderT Connection (ExceptT Error IO) a )
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Error,
            MonadReader Connection)

runBluetoothM :: BluetoothM a -> Connection -> IO (Either Error a)
runBluetoothM (BluetoothM e) conn = runExceptT $ runReaderT e conn

toBluetoothM :: (Connection -> IO (Either MethodError a)) -> BluetoothM a
toBluetoothM = BluetoothM . ReaderT . fmap (withExceptT DBusError . ExceptT)


-- * Assorted

-- | This datatype, which is kept opaque, is returned when an application is
-- successfully registered, and required as an argument from functions that
-- should only be called after the application has been registered.
newtype ApplicationRegistered = ApplicationRegistered ObjectPath
  deriving (Eq, Show, Generic)

data Status
  = Success
  | Failure
  deriving (Eq, Show, Read, Ord, Enum, Generic)


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
