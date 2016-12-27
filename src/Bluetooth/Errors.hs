{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
module Bluetooth.Errors where

import           Control.Monad.Except
import qualified Data.Text            as T
import           GHC.Exts             (Constraint)

-- All of this would be less verbose with overloaded labels, but then we
-- couldn't supported GHC < 8.

type ReadValue a = Handler '[ ThrowsFailed
                            , ThrowsInProgress
                            , ThrowsNotPermitted
                            , ThrowsNotAuthorized
                            , ThrowsNotSupported
                            ] a

type WriteValue a = Handler '[ ThrowsFailed
                             , ThrowsInProgress
                             , ThrowsNotPermitted
                             , ThrowsInvalidValueLength
                             , ThrowsNotAuthorized
                             , ThrowsNotSupported
                             ] a

newtype Handler (errs :: [(* -> *) -> Constraint]) a
  = Handler { getReadValue :: ExceptT T.Text IO a }
  -- NOT MonadError!
  deriving (Functor, Applicative, Monad, MonadIO)

type family IsElem (x :: k) (list :: [k]) :: Constraint where
  IsElem x (x ': xs) = ()
  IsElem x (y ': xs) = IsElem x xs

class ThrowsFailed m where errFailed :: m a
instance (ThrowsFailed `IsElem` errs) => ThrowsFailed (Handler errs) where
  errFailed = Handler $ throwError "org.bluez.Error.Failed"

class ThrowsInProgress m where errInProgress :: m a
instance (ThrowsInProgress `IsElem` errs) => ThrowsInProgress (Handler errs) where
  errInProgress = Handler $ throwError "org.bluez.Error.InProgress"

class ThrowsNotPermitted m where errNotPermitted :: m a
instance (ThrowsNotPermitted `IsElem` errs) => ThrowsNotPermitted (Handler errs) where
  errNotPermitted = Handler $ throwError "org.bluez.Error.NotPermitted"

class ThrowsNotAuthorized m where errNotAuthorized :: m a
instance (ThrowsNotAuthorized `IsElem` errs) => ThrowsNotAuthorized (Handler errs) where
  errNotAuthorized = Handler $ throwError "org.bluez.Error.NotAuthorized"

class ThrowsNotSupported m where errNotSupported :: m a
instance (ThrowsNotSupported `IsElem` errs) => ThrowsNotSupported (Handler errs) where
  errNotSupported = Handler $ throwError "org.bluez.Error.NotSupported"

class ThrowsInvalidValueLength m where errInvalidValueLength :: m a
instance (ThrowsInvalidValueLength `IsElem` errs) => ThrowsInvalidValueLength (Handler errs) where
  errInvalidValueLength = Handler $ throwError "org.bluez.Error.InvalidValueLength"
