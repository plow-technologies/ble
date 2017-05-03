#!/bin/bash

set -o errexit

python3 -m dbusmock --system org.bluez.Mock / org.bluez.Mock &
sleep 5
trap 'kill $(jobs -p)' EXIT

gdbus call --system -d org.bluez.Mock -o / \
  -m org.freedesktop.DBus.Mock.AddTemplate 'bluez5' '{}'

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddAdapter \
  'hci0' \
  'computer'

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddDevice \
  'hci0' \
  '11:22:33:44:55:66' \
  'Test Device'

gdbus call --system -d org.bluez.Mock -o / \
  -m org.bluez.Mock.AddGATTService \
  '/org/bluez/hci0' \
  '0x180D' \
  '4ea7235c-8d49-4a6f-abe6-1883218a93a7' \
  'true'

sleep 100000
