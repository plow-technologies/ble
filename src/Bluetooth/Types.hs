{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bluetooth.Types where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Word (Word16)
import Data.LargeWord (Word128)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT(ExceptT), MonadError, runExceptT)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT, MonadReader)
import DBus (MethodError, DBusConnection)
import Numeric (readHex)
import Data.String (IsString(fromString))

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

newtype BluetoothM a
  = BluetoothM ( ReaderT DBusConnection (ExceptT MethodError IO) a )
  deriving (Functor, Applicative, Monad, MonadIO, MonadError MethodError,
            MonadReader DBusConnection)

runBluetoothM :: BluetoothM a -> DBusConnection -> IO (Either MethodError a)
runBluetoothM (BluetoothM e) conn = runExceptT (runReaderT e conn)

toBluetoothM :: (DBusConnection -> IO (Either MethodError a)) -> BluetoothM a
toBluetoothM = BluetoothM . ReaderT . fmap ExceptT

data Error
