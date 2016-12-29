
-- | This module exports all you should need to build a Bluetooth Low Energy
--  (BLE) peripheral.
--
-- The core concepts involved are:
--
--    ['Application'] This contains the entirety of your application, and is
--    composed of zero or more @Service@s.
--
--    ['Service'] A set of zero or more conceptually related @Characteristic@s.
--    Identified by it's 'UUID'.
--
--    ['Characteristic'] @Characteristic@s represent the actual data of your
--    application. They may allow reading, writing, and subscribing. Also
--    identified by it's 'UUID'.
--
--    ['Advertisement'] This describes how an application will advertise itself
--    to other BLE devices.
--
-- All three have @IsString@ instances and lens field accessors.  The
-- recommended way of using this library is by using the @OverloadedStrings@
-- pragma and lenses. A complete example can be found
-- <https://github.com/plow-technologies/ble/blob/master/examples/Counter.hs here>.
--
module Bluetooth
  (
    registerApplication
  , registerAndAdvertiseApplication
  , advertise
  , advertisementFor
  , connect
  , runBluetoothM


  -- * Handler
  , ReadValueM
  , WriteValueM
  , Handler

  -- ** Handler error classes
  , ThrowsFailed(..)
  , ThrowsInProgress(..)
  , ThrowsNotPermitted(..)
  , ThrowsNotAuthorized(..)
  , ThrowsNotSupported(..)
  , ThrowsInvalidValueLength(..)


  -- * BLE Types
  -- | Types representing components of a BLE application.
  , Connection
  , Application(..)
  , Service(..)
  , UUID(UUID)
  , CharacteristicProperty(..)
  , Characteristic(..)
  , CharacteristicBS
  , Advertisement(..)

  -- * Field lenses
  , uuid
  , properties
  , readValue
  , writeValue
  , notifying
  , characteristics
  , services
  , path
  , type_
  , solicitUUIDs
  , serviceUUIDs
  , manufacturerData
  , serviceData
  , includeTxPower

  -- * Encoding and decogin
  -- | Helpers for 'readValue' and 'writeValue'.
  , encodeRead
  , encodeWrite

  -- * Re-exports
  , module Lens.Micro
  , module Lens.Micro.GHC
  ) where

import Bluetooth.Internal.DBus as X
import Bluetooth.Internal.Types as X
import Bluetooth.Internal.Serialize as X
import Bluetooth.Internal.Errors as X

import Lens.Micro
import Lens.Micro.GHC
