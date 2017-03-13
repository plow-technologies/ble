
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
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Bluetooth
-- > import Control.Concurrent (threadDelay)
-- >
-- > app :: Application
-- > app = "/com/turingjump/example" & services .~ [aService]
-- >
-- > aService :: Service
-- > aService = "d0bc6707-e9a5-4c85-8d22-d73d33f0330c"
-- >     & characteristics .~ [aCharacteristic]
-- >
-- > aCharacteristic :: CharacteristicBS
-- > aCharacteristic = "b3170df6-1770-4d60-86db-a487534cbcc3"
-- >     & readValue ?~ encodeRead (return (32::Int))
-- >     & properties .~ [CPRead]
-- >
-- > main :: IO ()
-- > main = do
-- >   conn <- connect
-- >   runBluetoothM (registerAndAdverstiseApplication app) conn
-- >   threadDelay maxBound
module Bluetooth
  (
    registerApplication
  , registerAndAdvertiseApplication
  , advertise
  , advertisementFor
  , connect
  , runBluetoothM

  -- * Field lenses
  , uuid
  , properties
  , readValue
  , writeValue
  , characteristics
  , services
  , path
  , type_
  , value
  , solicitUUIDs
  , serviceUUIDs
  , manufacturerData
  , serviceData
  , includeTxPower
  , connectionName

  -- * Notifications
  , characteristicIsNotifying
  , writeChrc



  -- * BLE Types
  -- | Types representing components of a BLE application.
  , Connection
  , Application
  , Service
  , UUID(UUID)
  , CharacteristicProperty(..)
  , Characteristic
  , CharacteristicBS
  , Advertisement
  , WithObjectPath


  -- * Encoding and decoding
  -- | Helpers for 'readValue' and 'writeValue'.
  , encodeRead
  , encodeWrite

  -- * Handler
  -- | @Handler@ is the monad BLE handlers run in.
  , Handler

  -- ** Handler errors
  , errorFailed
  , errorInProgress
  , errorNotPermitted
  , errorNotSupported
  , errorNotAuthorized
  , errorInvalidValueLength

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
