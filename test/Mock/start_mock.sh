#!/bin/bash

set -o errexit

# This script registers a mock service with DBus. The service contains one
# characteristic.

SERVICE_UUID=4ea7235c-8d49-4a6f-abe6-1883218a93a7
CHARACTERISTIC_UUID=6fe4afc7-ebf8-4369-90aa-0fe45064e3f9
CHARACTERISTIC_VALUE=3

python3 -m dbusmock --system org.bluez.Mock / org.bluez \
   2>&1 > /dev/null &

sleep 5
trap 'kill $(jobs -p)' EXIT

gdbus call --system -d org.bluez.Mock -o / \
  -m org.freedesktop.DBus.Mock.AddTemplate 'bluez5' '{}' \
  2>&1 > /dev/null

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddAdapter \
  'hci0' \
  'computer' 2>&1 > /dev/null

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddDevice \
  'hci0' \
  '11:22:33:44:55:66' \
  'Test Device' 2>&1 > /dev/null

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddGATTService \
  '/org/bluez/hci0' \
  '1' \
  "$SERVICE_UUID" \
  'true' 2>&1 > /dev/null

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddGATTCharacteristic \
  "/org/bluez/hci0/service0001" \
  '1' \
  "$CHARACTERISTIC_UUID" \
  '["read"]' \
  "$CHARACTERISTIC_VALUE" \
  'false' 2>&1 > /dev/null

printf "ready\n"

sleep 100000
