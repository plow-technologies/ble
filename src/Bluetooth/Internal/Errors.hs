{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
module Bluetooth.Internal.Errors where

import           Control.Monad.Except
import qualified Data.Text            as T
import           Data.Tuple           (swap)
import           GHC.Generics         (Generic)

-- | Errors a handler might throw. In general, you should only throw handlers
-- that are allowed for the handler type (read/write).
data Error
  -- | Can be throw from either read or write handlers.
  = ErrorFailed
  -- | Can be throw from either read or write handlers.
  | ErrorInProgress
  -- | Can be throw from either read or write handlers.
  | ErrorNotPermitted
  -- | Can be throw from either read or write handlers.
  | ErrorNotAuthorized
  -- | Can be throw from either read or write handlers.
  | ErrorNotSupported
  -- | Indicates that the argument has invalid length. Should not be used from
  -- a read handler
  | ErrorInvalidValueLength
  deriving (Eq, Show, Read, Generic, Ord, Bounded)


newtype Handler a
  = Handler { getReadValue :: ExceptT T.Text IO a }
  deriving (Functor, Applicative, Monad, MonadIO, Generic)

instance MonadError Error Handler where
  throwError x = case lookup x errorMapping of
    Just v -> Handler $ throwError v
    -- Should not happen
    Nothing -> Handler $ throwError "org.bluez.Error.Failed"
  catchError x c = case lookup x (swap <$> errorMapping) of
    Just v -> c v
    Nothing -> c ErrorFailed

errorMapping :: [(Error, T.Text)]
errorMapping =
  [ (ErrorFailed, "org.bluez.Error.Failed")
  , (ErrorInProgress, "org.bluez.Error.InProgress")
  , (ErrorNotPermitted, "org.bluez.Error.NotPermitted")
  , (ErrorNotAuthorized, "org.bluez.Error.NotAuthorized")
  , (ErrorNotSupported, "org.bluez.Error.NotSupported")
  , (ErrorInvalidValueLength, "org.bluez.Error.InvalidValueLength")
  ]
