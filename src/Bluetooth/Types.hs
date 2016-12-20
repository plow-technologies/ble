{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Bluetooth.Types where


import Control.Monad.Except (ExceptT(ExceptT), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT, MonadReader)
import DBus (MethodError, DBusConnection, ObjectPath, Representable(..),
             DBusType(TypeVariant, TypeDict, DBusSimpleType), DBusValue(DBVVariant, DBVDict), connectBus,
             ConnectionType(System), Object, objectRoot, DBusSimpleType(..), objectPath, objectPathToText)
import DBus.Types (root)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Singletons.TH (genSingletons)
import Data.String (IsString(fromString))
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Numeric (readHex, showHex)

import qualified Data.UUID as UUID
import qualified Data.Map as Map
import qualified Data.Text as T

-- See <http://www.itu.int/rec/T-REC-X.667/en ITU-T Rec. X.677> for more
-- information on the format and generation of these UUIDs.
data UUID
  = OfficialUUID16 Word16
  | OfficialUUID32 Word32
  -- Custom services use 128-bit identifiers
  | UnofficialUUID UUID.UUID
  deriving (Eq, Show, Generic)

instance IsString UUID where
  fromString x
    | length x > 8  = UnofficialUUID $ fromMaybe (error "UUID.fromString: invalid UUID") $ UUID.fromString x 
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
  

data Application = Application
  { applicationRoot :: ObjectPath
  , applicationServices :: [Service]
  } deriving (Eq, Show, Generic)

instance Representable Application where
  type RepType Application = 'TypeDict 'TypeObjectPath (RepType Service)
  toRep app = DBVDict $ go <$> applicationServices app
    where
      prefixed n = objectPath $ objectPathToText (applicationRoot app) <> "/" <> n
      go s = (toRep $ prefixed $ serviceName s , toRep s)
      
data Service = Service
  { serviceName :: T.Text
  , serviceUUID :: UUID
  , serviceCharacteristics :: [Characteristic]
  } deriving (Eq, Show, Generic)



instance Representable Service where
  type RepType Service = 'TypeDict 'TypeString ('TypeDict 'TypeString 'TypeVariant)
  toRep s = toRep (Map.fromList [(gattServiceIFace, tmap)])
    where
      gattServiceIFace :: T.Text
      gattServiceIFace = "org.bluez.GattService1"
      
      tmap :: Map.Map T.Text Any
      tmap = Map.fromList [ ("UUID", MkAny $ serviceUUID s)
                          -- Only primary services for now
                          , ("Primary", MkAny $ True)
                          , ("Characteristics", MkAny ([] :: [ObjectPath]))
                          ]
  
data Characteristic = Characteristic
  { characteristicName :: T.Text
  , characteristicUUID :: UUID
  , characteristicProperties :: [CharacteristicProperty]
  } deriving (Eq, Show, Generic)

data CharacteristicProperty
  = CPBroadcast
  | CPRead
  | CPWriteWithouResponse
  | CPWrite
  | CPNotify
  | CPIndicate
  | CPSignedWriteCommand
  deriving (Eq, Show, Read, Ord, Generic)

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

-- The constructor should not be exported.
data Connection = Connection
  { dbusConn :: DBusConnection
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

newtype BluetoothM a
  = BluetoothM ( ReaderT Connection (ExceptT MethodError IO) a )
  deriving (Functor, Applicative, Monad, MonadIO, MonadError MethodError,
            MonadReader Connection)

runBluetoothM :: BluetoothM a -> Connection -> IO (Either MethodError a)
runBluetoothM (BluetoothM e) conn = runExceptT $ runReaderT e conn

toBluetoothM :: (Connection -> IO (Either MethodError a)) -> BluetoothM a
toBluetoothM = BluetoothM . ReaderT . fmap ExceptT

-- | A Haskell existential type corresponding to DBus' @Variant@.
data Any where
  MkAny :: forall a . Representable a => a -> Any

instance Representable Any where
  type RepType Any = 'TypeVariant
  toRep (MkAny x) = DBVVariant (toRep x)
  fromRep (DBVVariant x) = Just (MkAny x)

-- $(genSingletons [''Characteristic, ''UUID, ''T.Text, ''Service])
