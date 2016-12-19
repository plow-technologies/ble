{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bluetooth.Types where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Word (Word16)
import Data.LargeWord (Word128)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT(ExceptT), MonadError, runExceptT)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT, MonadReader)
import DBus (MethodError, DBusConnection, ObjectPath, Representable(..), DBusType(TypeVariant), DBusValue(DBVVariant), SomeDBusValue)
import DBus.Message (MessageHeader)
import Numeric (readHex)
import Data.String (IsString(fromString))
import Control.Monad.Writer (WriterT, MonadWriter, runWriterT)
import Control.Monad.Trans (lift)

data UUID
  = OfficialUUID Word16
  -- Custom services use 128-bit identifiers
  | UnofficialUUID Word128
  deriving (Eq, Show, Generic)

instance IsString UUID where
  fromString x
    | length x == 32 = UnofficialUUID $ go x
    | length x == 4  = OfficialUUID $ go x
    | otherwise      = error "UUID.fromString: expecting 128 or 16-bit UUID"
    where
    go y = case [ z | (z, "") <- readHex y ] of
      [val] -> val
      []    -> error "UUID.fromString: no parse"
      _     -> error "UUID.fromString: ambiguous parse"

data Application = Application
  { applicationRoot :: ObjectPath
  , applicationServices :: [Service]
  } deriving (Eq, Show, Generic)
      
data Service = Service
  { serviceName :: T.Text
  , serviceUUID :: UUID
  , serviceCharacteristics :: [Characteristic]
  } deriving (Eq, Show, Generic)

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

data AdvertisingPacketType
  = ConnectableUndirected
  | ConnectableDirected
  | NonConnnectableUndirected
  | ScannableUndirected
  deriving (Eq, Show, Read, Generic, Ord)

data Handlers = Handlers
  { methodCallHandler :: MessageHeader -> [SomeDBusValue] -> IO ()
  , signalHandler     :: MessageHeader -> [SomeDBusValue] -> IO ()
  }

instance Monoid Handlers where
  mempty = let u _ _ = return () in Handlers u u
  Handlers f1 g1 `mappend` Handlers f2 g2 = Handlers (f1 >> f2) (g1 >> g2) 

newtype BluetoothM a
  = BluetoothM ( ReaderT DBusConnection (WriterT Handlers (ExceptT MethodError IO)) a )
  deriving (Functor, Applicative, Monad, MonadIO, MonadError MethodError,
            MonadReader DBusConnection)

runBluetoothM :: BluetoothM a -> DBusConnection -> IO (Either MethodError (a, Handlers))
runBluetoothM (BluetoothM e) conn = runExceptT $ runWriterT (runReaderT e conn)

-- toBluetoothM :: (DBusConnection -> IO (Either MethodError a)) -> BluetoothM a
-- toBluetoothM f =  BluetoothM $ ReaderT . fmap lift

-- | A Haskell existential type corresponding to DBus' @Variant@.
data Any where
  MkAny :: forall a . Representable a => a -> Any

instance Representable Any where
  type RepType Any = 'TypeVariant
  toRep (MkAny x) = DBVVariant (toRep x)
  fromRep (DBVVariant x) = Just (MkAny x)
