{-# LANGUAGE UndecidableInstances #-}
module Bluetooth.Serialize where

import qualified Data.ByteString as BS
import qualified Data.Serialize  as S

import Bluetooth.Errors

encodeRead :: S.Serialize a => ReadValueM a -> ReadValueM BS.ByteString
encodeRead h = S.encode <$> h

encodeWrite :: (S.Serialize a, S.Serialize b)
  => (a -> WriteValueM b) -> (BS.ByteString -> WriteValueM BS.ByteString)
encodeWrite h v = case S.decode v of
  Left _   -> errFailed
  Right v' -> S.encode <$> h v'
