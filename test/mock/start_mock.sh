#!/bin/bash

mkvirtualenv --python=$(which python3) ble-test
workon ble-test

pip3 install -r requirements.txt
python3 -m dbusmock --system org.bluez.Mock /org/bluez/Mock org.bluez.Mock

gdbus call --system -d org.bluez.Mock -o /org/bluez/Mock -m org.bluez.Mock.AddTemplate 'upower' '{"OnBattery": <true>}'
