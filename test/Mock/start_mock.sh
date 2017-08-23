#!/bin/bash

set -o errexit

# This script registers a mock service with DBus. The service contains one
# characteristic.


SERVICE_UUID=4ea7235c-8d49-4a6f-abe6-1883218a93a7
CHARACTERISTIC_UUID=6fe4afc7-ebf8-4369-90aa-0fe45064e3f9
CHARACTERISTIC_VALUE=3
LOG_TO=${BLE_TEST_LOG_FILE:-/dev/null}


while getopts l: opt; do
    case $opt in
        l) LOG_TO=$OPTARG ;;
        \?) printf "Invalid option: %s" "$OPTARG" ;;
    esac
done

# echo "Logging to $LOG_TO"

python3 -m dbusmock --system org.bluez.Mock / org.bluez \
   >> "$LOG_TO" 2>&1 &

sleep 5
trap 'kill $(jobs -p)' EXIT

gdbus call --system -d org.bluez.Mock -o / \
  -m org.freedesktop.DBus.Mock.AddTemplate 'bluez5' '{}' \
  >> "$LOG_TO" 2>&1

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddAdapter \
  'hci0' \
  'computer' >> "$LOG_TO" 2>&1

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddDevice \
  'hci0' \
  '11:22:33:44:55:66' \
  'Test Device' >> "$LOG_TO" 2>&1

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddGATTService \
  '/org/bluez/hci0' \
  '1' \
  "$SERVICE_UUID" \
  'true' >> "$LOG_TO" 2>&1

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddGATTCharacteristic \
  "/org/bluez/hci0/service0001" \
  '1' \
  "$CHARACTERISTIC_UUID" \
  '["read", "write"]' \
  "$CHARACTERISTIC_VALUE" \
  'false' >> "$LOG_TO" 2>&1

printf "ready\n"

sleep 100000

# scan
# connect to device
# scan
