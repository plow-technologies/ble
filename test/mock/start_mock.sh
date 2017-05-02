#!/bin/bash

set -o errexit

python3 -m dbusmock --system org.bluez.Mock /org/bluez/Mock org.bluez.Mock &
sleep 5
trap 'kill $(jobs -p)' EXIT

gdbus call --system -d org.bluez.Mock -o /org/bluez/Mock -m org.bluez.Mock.AddTemplate 'upower' '{"OnBattery": <true>}'

