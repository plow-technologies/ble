module Main (main) where

-- This examples contains a simple Characteristic which only allows encrypted
-- authenticated reads. If the device pairing was not authenticated and
-- encrypted, the characteristic cannot be read. The default bluetooth agent
-- can handle PIN authentication. Check that the relevant device is setup to
-- use encryption and authentication:
--
--  hciconfig <dev>      -- e.g hciconfig hci0
--
-- Or change it with e.g.:
--
--  hciconfig <dev> auth
--
import Bluetooth
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  conn <- connect
  x <- runBluetoothM (registerAndAdvertiseApplication app) conn
  case x of
    Right _ -> putStrLn "Started BLE auth application!"
    Left e -> error $ "Error starting application " ++ show e
  threadDelay maxBound


app :: Application
app
  = "/com/turingjump/example/auth"
    & services .~ [auth]

auth :: Service 'Local
auth
  = "18b2e7ec-2706-429e-a021-ab5e8158477b"
    & characteristics .~ [secret]

secret :: CharacteristicBS 'Local
secret
  = "5bf24762-d9d1-445f-b81d-87069cc35e36"
    & readValue ?~ encodeRead (return ("Juke/19" :: String))
    & properties .~ [CPEncryptAuthenticatedRead]
