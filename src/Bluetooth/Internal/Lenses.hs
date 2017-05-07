module Bluetooth.Internal.Lenses where

import Lens.Micro

class HasPath s a | s -> a where
  -- | Returns the `ObjectPath` of an entity.
  path :: Lens' s a
class HasValue s a | s -> a where
  -- | Returns the actual value of an entity that is wrapped in an
  value :: Lens' s a
class HasOffset s a | s -> a where
  -- | A `Word16` data offset.
  offset :: Lens' s a
class HasProperties s a | s -> a where
  -- | The properties of e.g. a characteristic.
  properties :: Lens' s a
class HasReadValue s a | s -> a where
  -- | Access the handler for reading a value, if there is one.
  readValue :: Lens' s a
class HasWriteValue s a | s -> a where
  -- | Access the handler for writing a value, if there is one. The handler
  -- should return `True` if the value was successfully update.
  writeValue :: Lens' s a
class HasUuid s a | s -> a where
  -- | The `UUID` of an entity
  uuid :: Lens' s a
class HasCharacteristics s a | s -> a where
  -- | An access for the list of characteristics.
  characteristics :: Lens' s a
class HasServices s a | s -> a where
  -- | An access for the list of services.
  services :: Lens' s a
class HasIncludeTxPower s a | s -> a where
  -- | Accessor for indicating whether an `Advertisement` announces TX power
  -- (transmission power).
  includeTxPower :: Lens' s a
class HasManufacturerData s a | s -> a where
  -- | Accessor for manufacting data.
  manufacturerData :: Lens' s a
class HasServiceData s a | s -> a where
  -- | Accessor for manufacting data.
  serviceData :: Lens' s a
class HasServiceUUIDs s a | s -> a where
  -- | Accessor for service UUIDs
  serviceUUIDs :: Lens' s a
class HasSolicitUUIDs s a | s -> a where
  -- | Accessor for solicit UUIDs. These are UUIDs that an application or
  -- service expects to be available.
  solicitUUIDs :: Lens' s a
class HasType_ s a | s -> a where
  type_ :: Lens' s a
