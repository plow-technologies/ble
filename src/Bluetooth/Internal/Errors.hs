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
import           GHC.Generics         (Generic)

newtype Handler a
  = Handler { getReadValue :: ExceptT T.Text IO a }
  deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError T.Text)

-- | Generic failure
errorFailed :: Handler a
errorFailed = throwError "org.bluez.Error.Failed"

errorInProgress :: Handler a
errorInProgress = throwError "org.bluez.Error.InProgress"

errorNotPermitted  :: Handler a
errorNotPermitted = throwError "org.bluez.Error.NotPermitted"

errorNotAuthorized :: Handler a
errorNotAuthorized = throwError "org.bluez.Error.NotAuthorized"

errorNotSupported :: Handler a
errorNotSupported = throwError "org.bluez.Error.NotSupported"

-- | Indicates that the argument has invalid length. Should not be used from
-- a read handler
errorInvalidValueLength :: Handler a
errorInvalidValueLength = throwError "org.bluez.Error.InvalidValueLength"
